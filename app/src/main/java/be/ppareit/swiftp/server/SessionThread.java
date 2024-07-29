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

import net.vrallev.android.cat.Cat;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.util.concurrent.ConcurrentHashMap;

import javax.net.ssl.SSLSocket;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.BuildConfig;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;
import be.ppareit.swiftp.utils.Logging;
import be.ppareit.swiftp.utils.AnonymousLimit;

public class SessionThread extends Thread {

    private static final int MAX_AUTH_FAILS = 3;
    public static final int DATA_CHUNK_SIZE = 65536;  // do file I/O in 64k chunks

    private static boolean[] selectedTypesCached = null;

    SSLSocket cmdSSLAuthSocket = null; // explicit tls socket
    SSLSocket cmdSSLSocket; // implicit tls socket
    Socket cmdSocket; // plain unencrypted socket
    private boolean pasvMode = false;
    private boolean binaryMode = false;
    private String userName = null;  // username that the client sends
    private boolean userAuthenticated = false;
    private File workingDir = FsSettings.getDefaultChrootDir();
    private File chrootDir = workingDir;
    private static ConcurrentHashMap<String, String> uriString = null; // scoped match user to perm
    private Socket dataSocket = null; // PASV plain data socket
    private SSLSocket sslDataSocket = null; // PASV secure data socket
    private File renameFrom = null;
    private LocalDataSocket localDataSocket;
    private InputStream dataInputStream = null;
    private OutputStream dataOutputStream = null;
    private boolean sendWelcomeBanner;
    // FTP control sessions should start out in ASCII, according to the RFC. However, many clients
    // don't turn on UTF-8 even though they support it, so we just turn it on by default.
    protected String encoding = "UTF-8";
    long offset = -1; // where to start append when using REST/RANG
    long endPosition = -1; // where to stop append when using RANG
    private String[] formatTypes = {"Size", "Modify", "Type", "Perm"}; // types option of MLST/MLSD
    private int authFails = 0;
    private String hashingAlgorithm = "SHA-1";
    private final Logging logging = new Logging();
    private boolean pbszEnabled = false;
    private boolean epsvEnabled = false;
    private boolean eprtEnabled = false;

    public SessionThread(Socket socket, LocalDataSocket dataSocket, SSLSocket sslSocket) {
        cmdSocket = socket;
        cmdSSLSocket = sslSocket;
        localDataSocket = dataSocket; // not an actual socket itself so name is misleading
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

        if (sslDataSocket != null && dataSocket == null) {
            if (!sslDataSocket.isConnected()) {
                Cat.i("Can't receive from unconnected socket");
                return -2;
            }

            try {
                do {
                    bytesRead = dataInputStream.read(buf, 0, buf.length);
                } while (bytesRead == 0);
            } catch (IOException e) {
                Cat.i("Error reading data socket");
                return 0;
            }
            return bytesRead;
        }

        if (dataSocket == null) {
            Cat.i("Can't receive from null dataSocket");
            return -2;
        }
        if (!dataSocket.isConnected()) {
            Cat.i("Can't receive from unconnected socket");
            return -2;
        }

        try {
            do {
                bytesRead = dataInputStream.read(buf, 0, buf.length);
            } while (bytesRead == 0);
        } catch (IOException e) {
            Cat.i("Error reading data socket");
            return 0;
        }
        return bytesRead;
    }

    /**
     * Called when we receive a PASV command.
     *
     * @return Whether the necessary initialization was successful.
     */
    public int onPasv() {
        return localDataSocket.onPasv(cmdSSLSocket != null || cmdSSLAuthSocket != null);
    }

    /*
    * Called when we receive a EPSV command.
    *
    * */
    public int onEpsv(InetAddress address) {
        return localDataSocket.onEpsv(address, cmdSSLSocket != null || cmdSSLAuthSocket != null);
    }

    /*
    * Called when we receive a EPRT commant.
    *
    * */
    public void onEprt(Inet6Address address, int port) {
        localDataSocket.onEprt(address, port);
    }

    /**
     * Called when we receive a PORT command.
     *
     * @return Whether the necessary initialization was successful.
     */
    public boolean onPort(InetAddress dest, int port) {
        return localDataSocket.onPort(dest, port);
    }

    /*
    * Returns IP of the device running Swiftp.
    * */
    public InetAddress getDataSocketPasvIp() {
        // When the client sends PASV, our reply will contain the address and port
        // of the data connection that the client should connect to. For this purpose
        // we always use the same IP address that the command socket is using.
        if (cmdSSLSocket != null) return cmdSSLSocket.getLocalAddress();
        if (cmdSSLAuthSocket != null) return cmdSSLAuthSocket.getLocalAddress();
        return cmdSocket.getLocalAddress();
    }

    public int getDataSocketPasvPort() {
        // When the client sends PASV, our reply will contain the address and port
        // of the data connection that the client should connect to. For this purpose
        // we always use the same IP address that the command socket is using.
        if (cmdSSLSocket != null) return cmdSSLSocket.getPort();
        if (cmdSSLAuthSocket != null) return cmdSSLAuthSocket.getPort();
        return cmdSocket.getPort();
    }

    /*
    * Returns IPv4 or IPv6 of client device.
    * IPv6 is returned as link local IP + network interface, when on same network.
    * */
    public String getRemoteAddress() {
        if (cmdSocket != null) return cmdSocket.getInetAddress().toString();
        else if (cmdSSLSocket != null) return cmdSSLSocket.getInetAddress().toString();
        else if (cmdSSLAuthSocket != null) return cmdSSLAuthSocket.getInetAddress().toString();
        return "";
    }

    /*
    * Returns true if the plain/explicit port is used or false if the implicit port is used.
    * */
    public boolean getIsPlainSocket() {
        if (cmdSSLAuthSocket != null) return false;
        else if (cmdSocket != null) return true;
        else if (cmdSSLSocket != null) return false;
        return false;
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
        if (cmdSSLSocket != null || cmdSSLAuthSocket != null) {
            try {
                sslDataSocket = localDataSocket.onTransferSSL();
                if (sslDataSocket == null) {
                    Cat.i("dataSocketFactory.onTransfer() returned null");
                    return false;
                }
                dataInputStream = sslDataSocket.getInputStream();
                dataOutputStream = sslDataSocket.getOutputStream();
                return true;
            } catch (IOException e) {
                Cat.i("IOException getting OutputStream for data socket");
                sslDataSocket = null;
                return false;
            }
        }

        try {
            dataSocket = localDataSocket.onTransfer();
            if (dataSocket == null) {
                Cat.i("dataSocketFactory.onTransfer() returned null");
                return false;
            }
            dataInputStream = dataSocket.getInputStream();
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
        if (dataInputStream != null) {
            try {
                dataInputStream.close();
            } catch (IOException ignore) {
            }
            dataInputStream = null;
        }
        if (dataOutputStream != null) {
            try {
                dataOutputStream.close();
            } catch (IOException ignore) {
            }
            dataOutputStream = null;
        }
        if (dataSocket != null) {
            try {
                dataSocket.close();
            } catch (IOException ignore) {
            }
            dataSocket = null;
        }
        if (sslDataSocket != null) {
            try {
                sslDataSocket.close();
            } catch (IOException ignore) {
            }
            sslDataSocket = null;
        }
    }

    public void quit() {
        Cat.d("SessionThread told to quit");
        closeSocket();
    }

    /**
     * Sanitize the logged commands so we don't leak username or password.
     */
    private String sanitizeCommand(String cmd) {
        // Don't sanitize in debug build
        if (BuildConfig.DEBUG) {
            return cmd;
        }
        // Running in release
        if (cmd.trim().startsWith("PASS")) {
            return "PASS [hidden]";
        } else if (cmd.trim().startsWith("USER")) {
            return "USER [hidden]";
        }
        return cmd;
    }

    @Override
    public void run() {
        Cat.i("SessionThread started");
        // Give client a welcome
        if (sendWelcomeBanner) {
            if (FsSettings.isBannerDisabled()) {
                new Logging().appendLog("Banner is disabled");
                writeString("220 ready\r\n");
            }
            else writeString("220 SwiFTP " + App.getVersion() + " ready\r\n");
        }
        logging.appendLog("Session started");
        // Main loop: read an incoming line and process it
        try {
            final Reader reader;
            Reader readerSSL = null;
            BufferedReader inSSL = null;
            if (cmdSSLSocket != null) reader = new InputStreamReader(cmdSSLSocket.getInputStream());
            else reader = new InputStreamReader(cmdSocket.getInputStream());
            final BufferedReader in = new BufferedReader(reader, 8192); // use 8k buffer
            final String ftps = "[ FTPS ] ";
            while (true) {
                String line;
                if (cmdSSLSocket != null) {
                    line = in.readLine();
                    logging.appendLog(ftps + line);
                } else if (cmdSSLAuthSocket != null) {
                    if (readerSSL == null) {
                        logging.appendLog("readerSSL is null *****");
                        readerSSL = new InputStreamReader(cmdSSLAuthSocket.getInputStream());
                        inSSL = new BufferedReader(readerSSL, 8192); // use 8k buffer
                    }
                    line = inSSL.readLine();
                    logging.appendLog(ftps + line);
                } else {
                    line = in.readLine(); // will accept \r\n or \n for terminator
                    logging.appendLog(line);
                }
                if (line != null) {
                    logging.appendLog(sanitizeCommand(line));
                    Cat.d("Received line from client: " + sanitizeCommand(line));
                    FtpCmd.dispatchCommand(this, line);
                } else {
                    logging.appendLog("quitting...");
                    Cat.i("readLine gave null, quitting");
                    break;
                }
            }
        } catch (IOException e) {
            logging.appendLog("Connection was dropped: " + e.getMessage());
            Cat.i("Connection was dropped");
        }
        closeSocket();
        AnonymousLimit.decrement();
        FsService.connWakelockEndHandler();
    }

    public void closeSocket() {
        logging.appendLog("Closing sockets...");
        if (cmdSocket == null && cmdSSLAuthSocket == null && cmdSSLSocket == null) {
            return;
        }
        try {
            if (cmdSocket != null) {
                cmdSocket.close();
            }
        } catch (Exception ignore) {
        }

        try {
            if (cmdSSLAuthSocket != null) {
                cmdSSLAuthSocket.close();
            }
        } catch (Exception ignore) {
        }

        try {
            if (cmdSSLSocket != null) {
                cmdSSLSocket.close();
            }
        } catch (Exception ignore) {
        }
    }

    public void writeBytes(byte[] bytes) {
        if (cmdSSLSocket != null) {
            try {
                OutputStream outputStream = cmdSSLSocket.getOutputStream();
                outputStream.write(bytes);
                outputStream.flush();
                localDataSocket.reportTraffic(bytes.length);
            } catch (IOException e) {
                Cat.i("Exception writing socket");
                closeSocket();
            }
            return;
        }
        if (cmdSSLAuthSocket != null) {
            try {
                OutputStream outputStream = cmdSSLAuthSocket.getOutputStream();
                outputStream.write(bytes);
                outputStream.flush();
                localDataSocket.reportTraffic(bytes.length);
            } catch (IOException e) {
                logging.appendLog("Error on auth socket output");
                Cat.i("Exception writing socket");
                closeSocket();
            }
            return;
        }
        if (cmdSocket != null) {
            try {
                OutputStream outputStream = cmdSocket.getOutputStream();
                outputStream.write(bytes);
                outputStream.flush();
                localDataSocket.reportTraffic(bytes.length);
            } catch (IOException e) {
                Cat.i("Exception writing socket");
                closeSocket();
            }
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

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserName() {
        return userName;
    }

    /**
     * @return true if we should allow FTP operations
     */
    public boolean isAuthenticated() {
        return userAuthenticated || FsSettings.allowAnonymous();
    }

    /**
     * @return true only when we are anonymously logged in
     */
    public boolean isAnonymouslyLoggedIn() {
        return !userAuthenticated && FsSettings.allowAnonymous();
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

    public String getHashingAlgorithm() {
        return hashingAlgorithm;
    }

    public void setHashingAlgorithm(String algorithm) {
        this.hashingAlgorithm = algorithm;
    }

    public File getChrootDir() {
        return chrootDir.isDirectory() ? chrootDir : FsSettings.getDefaultChrootDir();
    }

    public void setChrootDir(String chrootPath) {
        if (chrootPath == null)
            return;
        File chrootDir = new File(chrootPath);
        if (chrootDir.isDirectory()) {
            this.chrootDir = chrootDir;
            this.workingDir = chrootDir;
        }
    }

    public static String getUriString(String threadName) {
        if (uriString == null) return "";
        if (uriString.containsKey(threadName)) return uriString.get(threadName);
        return "";
    }

    public static void putUriString(String threadName, String s) {
        if (uriString == null) uriString = new ConcurrentHashMap<>();
        uriString.put(threadName, s);
    }

    public static void removeUriString(String threadName) {
        if (uriString == null) return;
        uriString.remove(threadName);
    }

    public boolean isPbszEnabled() {
        return pbszEnabled;
    }

    public void setPbszEnabled(boolean pbszEnabled) {
        this.pbszEnabled = pbszEnabled;
    }

    public boolean isEpsvEnabled() {
        return epsvEnabled;
    }

    public void setEpsvEnabled(boolean epsvEnabled) {
        this.epsvEnabled = epsvEnabled;
    }

    public boolean isEprtEnabled() {
        return eprtEnabled;
    }

    public void setEprtEnabled(boolean eprtEnabled) {
        this.eprtEnabled = eprtEnabled;
    }

    public String makeSelectedTypesResponse(FileUtil.Gen gen) {
        StringBuilder response = new StringBuilder();
        String[] selectedTypes = getFormatTypes();

        if (selectedTypesCached == null && selectedTypes != null) {
            selectedTypesCached = new boolean[formatTypes.length];
            for (String selectedType : selectedTypes) {
                switch (selectedType) {
                    case "Size":
                        selectedTypesCached[0] = true;
                        break;
                    case "Modify":
                        selectedTypesCached[1] = true;
                        break;
                    case "Type":
                        selectedTypesCached[2] = true;
                        break;
                    case "Perm":
                        selectedTypesCached[3] = true;
                        break;
                }
            }
        }

        final boolean isFile = gen.isFile();
        final boolean isDirectory = gen.isDirectory();

        if (selectedTypesCached[0]) {
            response.append("Size=").append(gen.length()).append(';');
        }
        if (selectedTypesCached[1]) {
            String timeStr = Util.getFtpDate(gen.lastModified());
            response.append("Modify=").append(timeStr).append(';');
        }
        if (selectedTypesCached[2]) {
            if (isFile) {
                response.append("Type=file;");
            } else if (isDirectory) {
                response.append("Type=dir;");
            }
        }
        if (selectedTypesCached[3]) {
            response.append("Perm=");
            if (gen.canRead()) {
                if (isFile) {
                    response.append('r');
                } else if (isDirectory) {
                    response.append("el");
                }
            }
            if (gen.canWrite()) {
                if (isFile) {
                    response.append("adfw");
                } else if (isDirectory) {
                    response.append("fpcm");
                }
            }
            response.append(';');
        }
        return response.toString();
    }
}
