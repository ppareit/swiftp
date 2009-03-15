package org.swiftp;

import java.net.InetAddress;

import android.content.Context;
import android.net.wifi.WifiManager;
import android.util.Log;

public class CmdPASV extends FtpCmd implements Runnable {
	//public static final String message = "TEMPLATE!!";
	
	public CmdPASV(SessionThread sessionThread, String input) {
		super(sessionThread, "PASV");
	}
	
	public void run() {
		myLog.l(Log.INFO, "PASV running");
		
		InetAddress myAddress = sessionThread.getLocalAddress();
		int port = sessionThread.openPasvSocket();
		if(port < 0) {
			// There was a problem opening a port
			myLog.l(Log.ERROR, "Couldn't open a port for PASV");
			sessionThread.writeString("502 Couldn't open a port\r\n");
			return;
		}
		String ipAsString = FTPServerService.getWifiIpAsString();
		ipAsString = ipAsString.replace('.',',');
		StringBuilder response = new StringBuilder("227 =");
		// Output our IP address in the format xxx,xxx,xxx,xxx
		response.append(ipAsString + ",");
		
		// Output our port in the format p1,p2 where port=p1*256+p2 
		response.append(port / 256);
		response.append(",");
		response.append(port % 256);
		String responseString = response.toString() + "\r\n";
		sessionThread.writeString(responseString);
		sessionThread.setPasvMode(true);
		myLog.l(Log.INFO, "PASV completed, sent: " + responseString);
	}
}
