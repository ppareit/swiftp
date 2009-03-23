package org.swiftp;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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
		ByteBuffer.allocate(Settings.getInputBufferSize());
	protected boolean pasvMode = false;
	protected boolean binaryMode = false;
	protected Account account = new Account();
	protected boolean authenticated = false;
	protected File prefix = new File("/");  // start off in the root
	protected ServerSocket dataServerSocket = null;
	protected Socket dataSocket = null;
	protected FTPServerService service;
	protected File renameFrom = null;
	/**
	 * Opens an incoming or outgoing socket depending on the value of pasvMode,
	 * then sends the given String over that socket. In all cases the socket is
	 * closed before returning.
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
			return false;
		}
	}
	
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
			if(acceptPasvSocket()) {
				myLog.l(Log.DEBUG, "initDataSocket success");
				return 1;
			} else {
				myLog.l(Log.INFO, "initDataSocket failure");
				return 0;
			}
		} else {
			String msg = "Non-PASV transfer unsupported";
			myLog.l(Log.ERROR, msg);
			return 2;
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
