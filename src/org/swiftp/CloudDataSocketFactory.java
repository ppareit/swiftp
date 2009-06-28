package org.swiftp;

import java.net.InetAddress;
import java.net.Socket;

public class CloudDataSocketFactory extends DataSocketFactory {
	/**
	 * Implements data socket connections that go through our proxy server
	 * out on the net. The proxy sits between the FTP client and us, the server.
	 * We have to build in some coordination between the server and proxy in order
	 * for data sockets to be handled properly. 
	 * 
	 * When we receive a "PASV" command from a client, we have to request that the
	 * proxy server open a port, accept a connection, and proxy all data on that
	 * socket between ourself and the FTP client.
	 * 
	 * When we receive a PORT command, we store the client's connection info,
	 * and when it's time to being transferring data, we request that the proxy
	 * make a connection to the client's IP & port and then proxy all data between
	 * ourself and the FTP client.
	 */
	
	
	public String getPasvIp() {
		
		return null;
	}

	public int getPortNumber() {
		// TODO Auto-generated method stub
		return 0;
	}

	public boolean onPasv() {
		// TODO Auto-generated method stub
		return false;
	}

	public boolean onPort(InetAddress dest, int port) {
		// TODO Auto-generated method stub
		return false;
	}

	public Socket onTransfer() {
		// TODO Auto-generated method stub
		return null;
	}

}
