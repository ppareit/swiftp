package org.swiftp;

import android.util.Log;

public class CmdQUIT extends FtpCmd implements Runnable {
	public static final String message = "TEMPLATE!!"; 
	
	public CmdQUIT(SessionThread sessionThread, String input) {
		super(sessionThread, CmdQUIT.class.toString());
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "QUITting");
		sessionThread.writeString("221 Goodbye\r\n");
		sessionThread.closeSocket();
	}

}
