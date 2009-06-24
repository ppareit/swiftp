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

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetAddress;
import java.net.Socket;
import java.nio.ByteBuffer;

import android.util.Log;

public class SessionThread extends Thread {
	protected boolean shouldExit = false;
	protected Socket socket;
	protected MyLog myLog = new MyLog(getClass().getName());
	protected ByteBuffer buffer = 
		ByteBuffer.allocate(Defaults.getInputBufferSize());
	protected boolean pasvMode = false;
	protected boolean binaryMode = false;
	protected Account account = new Account();
	protected boolean authenticated = false;
	protected File prefix = Globals.getChrootDir();
	//protected ServerSocket dataServerSocket = null;
	protected Socket dataSocket = null;
	//protected FTPServerService service;
	protected File renameFrom = null;
	//protected InetAddress outDataDest = null; 
	//protected int outDataPort = 20; // 20 is the default ftp-data port
	protected DataSocketFactory dataSocketFactory;
	OutputStream dataOutputStream = null;
	
	/**
	 * Used when we get a PORT command to open up an outgoing socket.
	 * @return
	 */
//	public void setPortSocket(InetAddress dest, int port) {
//		myLog.l(Log.DEBUG, "Setting PORT dest to " +
//				dest.getHostAddress() + " port " + port);
//		outDataDest = dest;
//		outDataPort = port;
//	}
	
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
	
	public boolean sendViaDataSocket(byte[] bytes, int len) {
		return sendViaDataSocket(bytes, 0, len);
	}
	
	/**
	 *  Sends a byte array over the already-established data socket
	 * @param bytes
	 * @param len
	 * @return
	 */
	public boolean sendViaDataSocket(byte[] bytes, int start, int len) {
		
		if(dataOutputStream == null) {
			myLog.l(Log.ERROR, "Can't send via null dataOutputStream");
			return false;
		}
		if(len == 0) {
			return true; // this isn't an "error" 
		}
		try {
			dataOutputStream.write(bytes, start, len);
		} catch(IOException e) {
			myLog.l(Log.INFO, "Couldn't write output stream for data socket");
			myLog.l(Log.INFO, e.toString());
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
		
		if(dataSocket == null) {
			myLog.l(Log.INFO, "Can't receive from null dataSocket");
			return -2;
		}
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
	 * Called when we receive a PASV command.
	 * @return Whether the necessary initialization was successful.
	 */
	public boolean onPasv() {
		return dataSocketFactory.onPasv();
	}
	
	/**
	 * Called when we receive a PORT command.
	 * @return Whether the necessary initialization was successful.
	 */
	public boolean onPort(InetAddress dest, int port) {
		return dataSocketFactory.onPort(dest, port);
	}
	
	public String getDataSocketPasvIp() {
		return dataSocketFactory.getPasvIp();
	}

	public int getDataSocketPort() {
		return dataSocketFactory.getPortNumber();
	}
	
	/**
	 * Will be called by (e.g.) CmdSTOR, CmdRETR, CmdLIST, etc. when they are about
	 * to start actually doing IO over the data socket.
	 * @return
	 */
	public boolean startUsingDataSocket() {
		try {
			dataSocket = dataSocketFactory.onTransfer();
			if(dataSocket == null) {
				myLog.l(Log.INFO, "dataSocketFactory.onTransfer() returned null");
				return false;
			}
			dataOutputStream = dataSocket.getOutputStream();
			return true;
		} catch (IOException e) {
			myLog.l(Log.INFO, "IOException getting OutputStream for data socket");
			dataSocket = null;
			return false;
		}
	}
	
//	public boolean initDataSocket() {
//		if (pasvMode) {
//			// In pasv mode, the socket is already open
//			/*if(acceptPasvSocket()) {
//				myLog.l(Log.DEBUG, "accept() success");
//				return 1;
//			} else {
//				myLog.l(Log.DEBUG, "accept() fail");
//				return 0;
//			}*/
//		} else {
//			if(outDataDest == null) {
//				myLog.l(Log.INFO, "pasvMode is false but no outDataDest set");
//				dataSocket = null;
//			}
//			dataSocket = dataSocketFactory.port(outDataDest, outDataPort);
//		}
//		if(dataSocket == null) {
//			return false;
//		} else {
//			return true;
//		}
//	}
	
	/*
	boolean connectOutDataSocket() {
		try {
			// If the user never specified an address, use the same one
			// that their other connection came from
			if(outDataDest == null) {
				outDataDest = socket.getInetAddress();
				myLog.l(Log.DEBUG, "Defaulting data address to " + 
						outDataDest.getHostAddress());
			}
			dataSocket = new Socket(outDataDest, outDataPort);
			return true;
		} catch(IOException e) {
			myLog.l(Log.INFO, "Couldn't open outbound data socket");
			return false;
		}
	}*/
	
	/**
	 * Opens a random port for an incoming data connection.
	 * @return The port number that is listening, or -1 on failure.
	 */
	/*public int openPasvSocket() {
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
	}*/
	

//	public boolean acceptPasvSocket() {
//		if(dataSocket != null) {
//			if(dataSocket.isConnected()) {
//				myLog.l(Log.INFO, "Can't accept pasv socket, already open");
//				return false;
//			}
//		}
//		try {
//			dataSocket = dataServerSocket.accept();
//		} catch (IOException e) {
//			myLog.l(Log.ERROR, "Couldn't accept pasv socket");
//			dataSocket = null;
//			return false;
//		}
//		return true;
//	}
	
	public void closeDataSocket() {
		myLog.l(Log.DEBUG, "Closing data socket");
		if(dataOutputStream != null) {
			try {
				dataOutputStream.close();
			} catch (IOException e) {}
			dataOutputStream = null;
		}
		if(dataSocket != null) {
			try {
				dataSocket.close();
			} catch(IOException e) {}
		}
		dataSocket = null;
	}
	
	protected InetAddress getLocalAddress() {
		return socket.getLocalAddress();
	}

	public void run() {
		myLog.l(Log.INFO, "SessionHandler started");
		
		writeString("220 SwiFTP ready\r\n");
		// Main loop: read an incoming line and process it
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(
					socket.getInputStream()), 8192); // use 8k buffer
			while(true) {
				/*buffer.clear();
				socket.
				//int bytesRead = socket.read(buffer);
				
				if(bytesRead == -1) {
					return;  // no more bytes can be read from channel, goodbye
				}
				buffer.flip();
				byte[] tempArr = new byte[buffer.limit()];
				System.arraycopy(buffer.array(), 0, tempArr, 0, 
						         buffer.limit());
				
				String asString = new String(tempArr, "UTF-8");
				
				
				String[] lines = asString.split("\n");
				for (String line : lines) {
					if(!Defaults.release) {
						Log.d("SessionThread", "Writing monitor: " + line);
					}
					FTPServerService.writeMonitor(true, line);
					FtpCmd.dispatchCommand(this, line);
				}
				
				buffer.flip();
				*/
				String line;
				line = in.readLine();  // will accept \r\n or \n for line terminator
				FTPServerService.writeMonitor(true, line);
				if(line != null) {
					myLog.l(Log.DEBUG, "Received line from client: " + line);
					FtpCmd.dispatchCommand(this, line);
				} else {
					myLog.l(Log.DEBUG, "Skipping null line");
				}
			}
		} catch (IOException e) {
			myLog.l(Log.INFO, "Client dropped connection");
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
			//socket.write(ByteBuffer.wrap(bytes)); // from old SocketChannel impln.
			BufferedOutputStream out = new BufferedOutputStream(socket.getOutputStream(),
					Defaults.dataChunkSize);
			out.write(bytes);
			out.flush();			
		} catch (IOException e) {
			myLog.l(Log.INFO, "Exception writing socket");
			closeSocket();
			return;
		}
	}
	
	public void writeString(String str) {
		FTPServerService.writeMonitor(false, str);
		writeBytes(str.getBytes());
	}
	
	protected Socket getSocket() {
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

//	public void setPasvMode(boolean pasvMode) {
//		this.pasvMode = pasvMode;
//	}

	//public SessionThread(SocketChannel socket, FTPServerService service) {
	public SessionThread(Socket socket, DataSocketFactory dataSocketFactory) {
		this.socket = socket;
		this.dataSocketFactory = dataSocketFactory;
		//this.service = service;
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
		try {
			this.prefix = prefix.getCanonicalFile().getAbsoluteFile();
		} catch (IOException e) {
			myLog.l(Log.INFO, "SessionThread canonical error");
		}
	}

	/*public FTPServerService getService() {
		return service;
	}

	public void setService(FTPServerService service) {
		this.service = service;
	}*/

	public Socket getDataSocket() {
		return dataSocket;
	}

	public void setDataSocket(Socket dataSocket) {
		this.dataSocket = dataSocket;
	}

//	public ServerSocket getServerSocket() {
//		return dataServerSocket;
//	}

	public File getRenameFrom() {
		return renameFrom;
	}

	public void setRenameFrom(File renameFrom) {
		this.renameFrom = renameFrom;
	}
}
