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

package org.swiftp;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import android.util.Log;

public class SessionThread extends Thread {
	protected boolean shouldExit = false;
	protected SocketChannel socket;
	protected MyLog myLog = new MyLog(getClass().getName());
	protected ByteBuffer buffer = 
		ByteBuffer.allocate(Defaults.getInputBufferSize());
	protected boolean pasvMode = false;
	protected boolean binaryMode = false;
	protected Account account = new Account();
	protected boolean authenticated = false;
	protected File prefix = new File("/");  // start off in the root
	protected ServerSocket dataServerSocket = null;
	protected Socket dataSocket = null;
	protected FTPServerService service;
	protected File renameFrom = null;
	protected InetAddress outDataDest = null; 
	protected int outDataPort = 20; // 20 is the default ftp-data port
	
	/**
	 * Used when we get a PORT command to open up an outgoing socket.
	 * @return
	 */
	public void setPortSocket(InetAddress dest, int port) {
		myLog.l(Log.DEBUG, "Setting PORT dest to " +
				dest.getHostAddress() + " port " + port);
		outDataDest = dest;
		outDataPort = port;
	}
	
	/**
	 * Sends a string over the already-established data socket
	 * 
	 * @param string
	 * @return Whether the send completed successfully
	 */
	public boolean sendViaDataSocket(String string) {
		try {
			byte[] bytes = string.getBytes("UTF-8");
			return sendViaDataSocket(bytes, bytes.length);
		} catch(UnsupportedEncodingException e) {
			// There's no plausible way this can happen
			myLog.l(Log.ERROR, "UTF-8 output failure");
			return false;
		}
	}
	
	/**
	 *  Sends a byte array over the already-established data socket
	 * @param bytes
	 * @param len
	 * @return
	 */
	public boolean sendViaDataSocket(byte[] bytes, int len) {
		if(!dataSocket.isConnected()) {
			myLog.l(Log.ERROR, "Can't send via unconnected socket");
			return false;
		}
		OutputStream out;
		try {
			out = dataSocket.getOutputStream();
			out.write(bytes, 0, len);
		} catch(IOException e) {
			try {
				throw new Exception("Ohno");
			} catch(Exception ex) {
				myLog.l(Log.DEBUG, "Stack trace: ");
				for (StackTraceElement element : ex.getStackTrace()) {
					myLog.l(Log.DEBUG, element.toString());
				}
			}
			myLog.l(Log.INFO, "Couldn't write output stream for data socket");
			return false;
		}
		return true;
	}
	
	/**
	 * Received some bytes from the data socket, which is assumed to already 
	 * be connected. The bytes are placed in the given array, and the number
	 * of bytes successfully read is returned. 
	 * @param bytes Where to place the input bytes
	 * @return >0 if successful which is the number of bytes read, -1 if no 
	 * bytes remain to be read, -2 if the data socket was not connected,
	 * 0 if there was a read error
	 */
	public int receiveFromDataSocket(byte[] buf) {
		int bytesRead;
		
		if(!dataSocket.isConnected()) {
			myLog.l(Log.INFO, "Can't receive from unconnected socket");
			return -2;
		}
		InputStream in;
		try {
			in = dataSocket.getInputStream();
			// If the read returns 0 bytes, the stream is not yet
			// closed, but we just want to read again.
			while((bytesRead = in.read(buf, 0, buf.length)) == 0) {}
			if(bytesRead == -1) {
				// If InputStream.read returns -1, there are no bytes
				// remaining, so we return 0.
				return -1;
			}
		} catch(IOException e) {
			myLog.l(Log.INFO, "Error reading data socket");
			return 0;
		}
		return bytesRead;
	}
	
	/**
	 * Prepares the data socket for transmission. The action taken depends on
	 * the value of pasvMode. Currently, only pasvMode == true is supported.
	 * @return 0 if the socket open failed, 1 if the socket open was successful,
	 * 2 if pasvMode was not true
	 */
	public int initDataSocket() {
		if (pasvMode) {
			myLog.l(Log.DEBUG, "to accept() data connection");
			if(acceptPasvSocket()) {
				myLog.l(Log.DEBUG, "accept() success");
				return 1;
			} else {
				myLog.l(Log.DEBUG, "accept() fail");
				return 0;
			}
		} else {
			if(connectOutDataSocket()) {
				return 1;
			} else {
				return 0;
			}
		}
	}
	
	boolean connectOutDataSocket() {
		try {
			// If the user never specified an address, use the same one
			// that their other connection came from
			if(outDataDest == null) {
				outDataDest = socket.socket().getInetAddress();
				myLog.l(Log.DEBUG, "Defaulting data address to " + 
						outDataDest.getHostAddress());
			}
			dataSocket = new Socket(outDataDest, outDataPort);
			return true;
		} catch(IOException e) {
			myLog.l(Log.INFO, "Couldn't open outbound data socket");
			return false;
		}
	}
	
	/**
	 * Opens a random port for an incoming data connection.
	 * @return The port number that is listening, or -1 on failure.
	 */
	public int openPasvSocket() {
		closeDataSocket();
		if(dataServerSocket != null) {
			if(dataServerSocket.isBound()) {
				try {
					myLog.l(Log.DEBUG, "Closing dataServerSocket");
					dataServerSocket.close();
				} catch(IOException e) {}
			}
		}

		// Listen on any port (0) with a backlog of 1
		ServerSocket server;
		try {
			server = new ServerSocket(0, 1);
		} catch(IOException e) {
			myLog.l(Log.ERROR, "Data socket creation error");
			return -1;
		}
		dataServerSocket = server;
		myLog.l(Log.DEBUG, "Data socket creation success");
		return server.getLocalPort();
	}
	

	public boolean acceptPasvSocket() {
		if(dataSocket != null) {
			if(dataSocket.isConnected()) {
				myLog.l(Log.ERROR, "Can't accept pasv socket, already open");
				return false;
			}
		}
		try {
			dataSocket = dataServerSocket.accept();
		} catch (IOException e) {
			myLog.l(Log.ERROR, "Couldn't accept pasv socket");
			dataSocket = null;
			return false;
		}
		return true;
	}
	
	public void closeDataSocket() {
		myLog.l(Log.DEBUG, "Closing data socket");
		if(dataSocket != null) {
			try {
				dataSocket.close();
			} catch(IOException e) {}
		}
		dataSocket = null;
	}
	
	protected InetAddress getLocalAddress() {
		return socket.socket().getLocalAddress();
	}

	public void run() {
		myLog.l(Log.INFO, "SessionHandler started");
		
		writeBytes(Responses.welcomeMsg);
		// Main loop: read an incoming line and process it
		try {
			while(true) {
				buffer.clear();
				int bytesRead = socket.read(buffer);
				
				if(bytesRead == -1) {
					return;  // no more bytes can be read from channel, goodbye
				}
				buffer.flip();
				byte[] tempArr = new byte[buffer.limit()];
				System.arraycopy(buffer.array(), 0, tempArr, 0, 
						         buffer.limit());
				
				String asString = new String(tempArr, "UTF-8");
				
				// A single read from the OS may have returned multiple command
				// lines. So we break it up on "\n". Correct clients will use 
				// the string "\r\n" to end their lines, but some only use 
				// "\n". FtpCmd.getParameter() can handle the "\r" if it's 
				// still there.
				
				String[] lines = asString.split("\n");
				for (String line : lines) {
					FTPServerService.writeMonitor(true, line);
					FtpCmd.dispatchCommand(this, line);
				}
				buffer.flip();
			}
		} catch (IOException e) {
			myLog.l(Log.ERROR, "IOException in main read");
		}
	}
	
	/**
	 *  A static method to check the equality of two byte arrays, but only up
	 * to a given length.
	 */
	public static boolean compareLen(byte[] array1, byte[] array2, int len) {
		for (int i=0; i<len; i++) {
			if(array1[i] != array2[i]) {
				return false;
			}
		}
		return true;
	}
	
	public void closeSocket() {
		if(socket == null) {
			return;
		}
		try {
			socket.close();  // no effect if the socket is already closed
		} catch (IOException e) {}
	}
		
	public void writeBytes(byte[] bytes) {
		try {
			socket.write(ByteBuffer.wrap(bytes));
			// todo: flush here?
		} catch (IOException e) {
			myLog.l(Log.ERROR, "Exception writing socket");
			closeSocket();
			return;
		}
	}
	
	public void writeString(String str) {
		FTPServerService.writeMonitor(false, str);
		writeBytes(str.getBytes());
	}
	
	protected SocketChannel getSocket() {
		return socket;
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

	public void setPasvMode(boolean pasvMode) {
		this.pasvMode = pasvMode;
	}

	public SessionThread(SocketChannel socket, FTPServerService service) {
		this.socket = socket;
		this.service = service;
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

	
	public boolean isAuthenticated() {
		return authenticated;
	}

	public void setAuthenticated(boolean authenticated) {
		if(authenticated) {
			myLog.l(Log.INFO, "Authentication complete");
		}
		this.authenticated = authenticated;
	}

	public File getPrefix() {
		return prefix;
	}

	public void setPrefix(File prefix) {
		this.prefix = prefix;
	}

	public FTPServerService getService() {
		return service;
	}

	public void setService(FTPServerService service) {
		this.service = service;
	}

	public Socket getDataSocket() {
		return dataSocket;
	}

	public void setDataSocket(Socket dataSocket) {
		this.dataSocket = dataSocket;
	}

	public ServerSocket getServerSocket() {
		return dataServerSocket;
	}

	public File getRenameFrom() {
		return renameFrom;
	}

	public void setRenameFrom(File renameFrom) {
		this.renameFrom = renameFrom;
	}
}
