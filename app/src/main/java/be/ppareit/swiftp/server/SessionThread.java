/*
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
 */

package be.ppareit.swiftp.server;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;

import net.vrallev.android.cat.Cat;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.Defaults;
import be.ppareit.swiftp.FsSettings;

public class SessionThread extends Thread {

    private Socket cmdSocket;
    private boolean pasvMode = false;
    private boolean binaryMode = false;
    private Account account = new Account();
    private boolean userAuthenticated = false;
    private File workingDir = FsSettings.getChrootDir();
    private Socket dataSocket = null;
    private File renameFrom = null;
    private LocalDataSocket localDataSocket;
    private OutputStream dataOutputStream = null;
    private boolean sendWelcomeBanner;
    protected String encoding = Defaults.SESSION_ENCODING;
    long offset = -1; // where to start append when using REST
    private String[] formatTypes = {"Size", "Modify", "Type", "Perm"}; // types option of MLST/MLSD
    private int authFails = 0;

    private final static int MAX_AUTH_FAILS = 3;

    public SessionThread(Socket socket, LocalDataSocket dataSocket) {
        cmdSocket = socket;
        localDataSocket = dataSocket;
        sendWelcomeBanner = true;
    }

    /**
     * Sends a string over the already-established data socket
     *
     * @param string string to send
     * @return Whether the send completed successfully
     */
    public boolean sendViaDataSocket(String string) {
        try {
            byte[] bytes = string.getBytes(encoding);
            Cat.d("Using data connection encoding: " + encoding);
            return sendViaDataSocket(bytes, 0, bytes.length);
        } catch (UnsupportedEncodingException e) {
            Cat.e("Unsupported encoding for data socket send");
            return false;
        }
    }

    /**
     * Sends a byte array over the already-established data socket
     *
     * @param bytes bytes to send
     * @param start start offset of data
     * @param len   number of bytes to write
     * @return true if success
     */
    public boolean sendViaDataSocket(byte[] bytes, int start, int len) {

        if (dataOutputStream == null) {
            Cat.i("Can't send via null dataOutputStream");
            return false;
        }
        if (len == 0) {
            return true; // this isn't an "error"
        }
        try {
            dataOutputStream.write(bytes, start, len);
        } catch (IOException e) {
            Cat.e("Couldn't write output stream for data socket, error:" + e.toString());
            return false;
        }
        localDataSocket.reportTraffic(len);
        return true;
    }

    /**
     * Received some bytes from the data socket, which is assumed to already be connected.
     * The bytes are placed in the given array buf, and the number of bytes successfully read
     * is returned.
     *
     * @param buf Where to place the input bytes
     * @return >0 if successful which is the number of bytes read
     *         -1 if no bytes remain to be read
     *         -2 if the data socket was not connected
     *         0 if there was a read  error
     */
    public int receiveFromDataSocket(byte[] buf) {
        int bytesRead;

        if (dataSocket == null) {
            Cat.i("Can't receive from null dataSocket");
            return -2;
        }
        if (!dataSocket.isConnected()) {
            Cat.i("Can't receive from unconnected socket");
            return -2;
        }
        InputStream in;
        try {
            in = dataSocket.getInputStream();
            // If the read returns 0 bytes, the stream is not yet
            // closed, but we just want to read again.
            while ((bytesRead = in.read(buf, 0, buf.length)) == 0) {
            }
            if (bytesRead == -1) {
                // If InputStream.read returns -1, there are no bytes remaining
                return -1;
            }
        } catch (IOException e) {
            Cat.i("Error reading data socket");
            return 0;
        }
        localDataSocket.reportTraffic(bytesRead);
        return bytesRead;
    }

    /**
     * Called when we receive a PASV command.
     *
     * @return Whether the necessary initialization was successful.
     */
    public int onPasv() {
        return localDataSocket.onPasv();
    }

    /**
     * Called when we receive a PORT command.
     *
     * @return Whether the necessary initialization was successful.
     */
    public boolean onPort(InetAddress dest, int port) {
        return localDataSocket.onPort(dest, port);
    }

    public InetAddress getDataSocketPasvIp() {
        // When the client sends PASV, our reply will contain the address and port
        // of the data connection that the client should connect to. For this purpose
        // we always use the same IP address that the command socket is using.
        return cmdSocket.getLocalAddress();
    }

    /**
     * Will be called by (e.g.) CmdSTOR, CmdRETR, CmdLIST, etc. when they are about to
     * start actually doing IO over the data socket.
     *
     * Must call closeDataSocket() when done
     *
     * @return true if successful
     */
    public boolean openDataSocket() {
        try {
            dataSocket = localDataSocket.onTransfer();
            if (dataSocket == null) {
                Cat.i("dataSocketFactory.onTransfer() returned null");
                return false;
            }
            dataOutputStream = dataSocket.getOutputStream();
            return true;
        } catch (IOException e) {
            Cat.i("IOException getting OutputStream for data socket");
            dataSocket = null;
            return false;
        }
    }

    /**
     * Call when done doing IO over the data socket
     */
    public void closeDataSocket() {
        Cat.d("Closing data socket");
        if (dataOutputStream != null) {
            try {
                dataOutputStream.close();
            } catch (IOException e) {
                /* swallow */
            }
            dataOutputStream = null;
        }
        if (dataSocket != null) {
            try {
                dataSocket.close();
            } catch (IOException e) {
                /* swallow */
            }
        }
        dataSocket = null;
    }

    public void quit() {
        Cat.d("SessionThread told to quit");
        closeSocket();
    }

    protected InetAddress getLocalAddress() {
        return cmdSocket.getLocalAddress();
    }

    @Override
    public void run() {
        Cat.i("SessionThread started");
        // Give client a welcome
        if (sendWelcomeBanner) {
            writeString("220 SwiFTP " + App.getVersion() + " ready\r\n");
        }
        // Main loop: read an incoming line and process it
        try {
            final Reader reader = new InputStreamReader(cmdSocket.getInputStream());
            final BufferedReader in = new BufferedReader(reader, 8192); // use 8k buffer
            while (true) {
                String line;
                line = in.readLine(); // will accept \r\n or \n for terminator
                if (line != null) {
                    Cat.d("Received line from client: " + line);
                    FtpCmd.dispatchCommand(this, line);
                } else {
                    Cat.i("readLine gave null, quitting");
                    break;
                }
            }
        } catch (IOException e) {
            Cat.i("Connection was dropped");
        }
        closeSocket();
    }

    public void closeSocket() {
        if (cmdSocket == null) {
            return;
        }
        try {
            cmdSocket.close();
        } catch (IOException e) {
        }
    }

    public void writeBytes(byte[] bytes) {
        try {
            // TODO: do we really want to do all of this on each write? Why?
            BufferedOutputStream out = new BufferedOutputStream(
                    cmdSocket.getOutputStream(), Defaults.dataChunkSize);
            out.write(bytes);
            out.flush();
            localDataSocket.reportTraffic(bytes.length);
        } catch (IOException e) {
            Cat.i("Exception writing socket");
            closeSocket();
            return;
        }
    }

    public void writeString(String str) {
        byte[] strBytes;
        try {
            strBytes = str.getBytes(encoding);
        } catch (UnsupportedEncodingException e) {
            Cat.e("Unsupported encoding: " + encoding);
            strBytes = str.getBytes();
        }
        writeBytes(strBytes);
    }

    protected Socket getSocket() {
        return cmdSocket;
    }

    public Account getAccount() {
        return account;
    }

    public void setAccount(Account account) {
        this.account = account;
    }

    public boolean isPasvMode() {
        return pasvMode;
    }

    static public ByteBuffer stringToBB(String s) {
        return ByteBuffer.wrap(s.getBytes());
    }

    public boolean isBinaryMode() {
        return binaryMode;
    }

    public void setBinaryMode(boolean binaryMode) {
        this.binaryMode = binaryMode;
    }

    /**
     * @return true if we should allow FTP opperations
     */
    public boolean isAuthenticated() {
        if (userAuthenticated == true || FsSettings.allowAnoymous() == true) {
            return true;
        }
        return false;
    }

    /**
     * @return true only when we are anonymously logged in
     */
    public boolean isAnonymouslyLoggedIn() {
        if (userAuthenticated == true) {
            return false;
        }
        if (FsSettings.allowAnoymous() == true) {
            return true;
        }
        return false;
    }

    /**
     * @return true if a valid user has logged in
     */
    public boolean isUserLoggedIn() {
        return userAuthenticated;
    }

    public void authAttempt(boolean authenticated) {
        if (authenticated) {
            Cat.i("Authentication complete");
            userAuthenticated = true;
        } else {
            authFails++;
            Cat.i("Auth failed: " + authFails + "/" + MAX_AUTH_FAILS);
            if (authFails > MAX_AUTH_FAILS) {
                Cat.i("Too many auth fails, quitting session");
                quit();
            }
        }
    }

    public File getWorkingDir() {
        return workingDir;
    }

    public void setWorkingDir(File workingDir) {
        try {
            this.workingDir = workingDir.getCanonicalFile().getAbsoluteFile();
        } catch (IOException e) {
            Cat.i("SessionThread canonical error");
        }
    }

    public Socket getDataSocket() {
        return dataSocket;
    }

    public void setDataSocket(Socket dataSocket) {
        this.dataSocket = dataSocket;
    }

    public File getRenameFrom() {
        return renameFrom;
    }

    public void setRenameFrom(File renameFrom) {
        this.renameFrom = renameFrom;
    }

    public String getEncoding() {
        return encoding;
    }

    public void setEncoding(String encoding) {
        this.encoding = encoding;
    }

    public String[] getFormatTypes() {
        return formatTypes;
    }

    public void setFormatTypes(String[] formatTypes) {
        this.formatTypes = formatTypes;
    }
}
