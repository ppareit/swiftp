package org.swiftp;

import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

import android.util.Log;

public class NormalDataSocketFactory extends DataSocketFactory {
	/**
	 * This class implements normal, traditional opening and closing of data sockets
	 * used for transmitting directory listings and file contents. PORT and PASV
	 * work according to the FTP specs. This is in contrast to a 
	 * CloudDataSocketFactory, which performs contortions to allow data sockets
	 * to be proxied through a server out in the cloud.
	 * 
	 */
	
	// Listener socket used for PASV mode
	ServerSocket server = null;
	// Remote IP & port information used for PORT mode
	InetAddress remoteAddr;
	int remotePort;
	
	public NormalDataSocketFactory() {
		clearState();
	}
		
	
	private void clearState() {
		/**
		 * Clears the state of this object, as if no pasv() or port() had occurred.
		 * All sockets are closed.
		 */
		if(server != null) {
			try {
				server.close();
			} catch (IOException e) {}
		}
		server = null;
		remoteAddr = null;
		remotePort = 0;
		myLog.l(Log.DEBUG, "NormalDataSocketFactory state cleared");
	}
	
	public boolean onPasv() {
		clearState();
		try {
			// Listen on any port (port parameter 0)
			server = new ServerSocket(0, Defaults.tcpConnectionBacklog);
			myLog.l(Log.DEBUG, "Data socket pasv() listen successful");
			return true;
		} catch(IOException e) {
			myLog.l(Log.ERROR, "Data socket creation error");
			clearState();
			return false;
		}
	}

	public boolean onPort(InetAddress remoteAddr, int remotePort) {
		clearState();
		myLog.l(Log.DEBUG, "Data Socket port() accept stub");
		this.remoteAddr = remoteAddr;
		this.remotePort = remotePort;
		return false;
	}
	
	public Socket onTransfer() {
		if(server == null) {
			// We're in PORT mode (not PASV)
			if(remoteAddr == null || remotePort == 0) {
				myLog.l(Log.INFO, "PORT mode but no remote data given");
				clearState();
				return null;
			}
			Socket socket;
			try {
				socket = new Socket(remoteAddr, remotePort);
			} catch (IOException e) {
				myLog.l(Log.INFO, 
						"Couldn't open PORT data socket to: " +
						remoteAddr.toString() + ":" + remotePort);
				clearState();
				return null;
			}
			return socket;
		} else {
			// We're in PASV mode (not PORT)
			Socket socket = null;
			try {
				socket = server.accept();
				myLog.l(Log.DEBUG, "onTransfer pasv accept successful");
			} catch (Exception e) {
				myLog.l(Log.INFO, "Exception accepting PASV socket");
				socket = null;
			}
			clearState();
			return socket;  // will be null if error occurred
		}
	}
	
	/**
	 * Return the port number that the remote client should be informed of (in the body
	 * of the PASV response).
	 * @return The port number, or -1 if error.
	 */
	public int getPortNumber() {
		if(server != null) {
			return server.getLocalPort(); // returns -1 if serversocket is unbound 
		} else {
			return -1;
		}
	}
	
	public String getPasvIp() {
		//String retVal = server.getInetAddress().getHostAddress();
		String retVal = FTPServerService.getWifiIpAsString();
		myLog.l(Log.DEBUG, String.format("getPasvIp() returning %s", retVal));
		return retVal;  
	}
}