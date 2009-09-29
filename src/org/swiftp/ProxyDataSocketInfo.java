package org.swiftp;

import java.net.Socket;

public class ProxyDataSocketInfo extends Socket {
	private int remotePublicPort;
	private Socket socket;
	
	public Socket getSocket() {
		return socket;
	}

	public void setSocket(Socket socket) {
		this.socket = socket;
	}

	public ProxyDataSocketInfo(Socket socket, int remotePublicPort) {
		this.remotePublicPort = remotePublicPort;
		this.socket = socket;
	}
	
	public int getRemotePublicPort() {
		return remotePublicPort;
	}

	public void setRemotePublicPort(int remotePublicPort) {
		this.remotePublicPort = remotePublicPort;
	}
	
}
