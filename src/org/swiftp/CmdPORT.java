package org.swiftp;

import java.net.InetAddress;
import java.net.UnknownHostException;

import android.util.Log;

public class CmdPORT extends FtpCmd implements Runnable {
	//public static final String message = "TEMPLATE!!";
	String input;
	
	public CmdPORT(SessionThread sessionThread, String input) {
		super(sessionThread, CmdPORT.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "PORT running");
		String errString = null;
		mainBlock: {
			String param = getParameter(input);
			String[] substrs = param.split(",");
			if(substrs.length != 6) {
				errString = "550 Malformed PORT argument\r\n";
				break mainBlock;
			}
			for(int i=0; i<substrs.length; i++) {
				// Check that each IP/port octet is numeric and not too long
				if(!substrs[i].matches("[0-9]+") || 
				    substrs[i].length() > 3) 
				{
					errString = "550 Invalid PORT argument: " + substrs[i] + 
								"\r\n";
					break mainBlock;
				}
			}
			byte[] ipBytes = new byte[4];
			for(int i=0; i<4; i++) {
				try {
					// We have to manually convert unsigned to signed
					// byte representation.
					int ipByteAsInt = Integer.parseInt(substrs[i]);
					if(ipByteAsInt >= 128) {
						ipByteAsInt -= 256;
					}
					ipBytes[i] = (byte)ipByteAsInt;
				} catch (Exception e) {
					errString = "550 Invalid PORT format: " 
						+ substrs[i] + "\r\n";
					break mainBlock;
				}
			}
			InetAddress inetAddr;
			try {
				inetAddr = InetAddress.getByAddress(ipBytes);
			} catch (UnknownHostException e) {
				errString = "550 Unknown host\r\n";
				break mainBlock;
			}
			
			int port = Integer.parseInt(substrs[4]) * 256 + 
			           Integer.parseInt(substrs[5]);
			
			sessionThread.setPortSocket(inetAddr, port);
			sessionThread.setPasvMode(false);
		}
		if(errString == null) {
			sessionThread.writeString("200 PORT OK\r\n");
		} else {
			myLog.l(Log.INFO, "PORT error: " + errString);
			sessionThread.writeString(errString);
		}
		myLog.l(Log.DEBUG, "PORT completed");
	}
}
