package org.swiftp;

import android.util.Log;

public class CmdPWD extends FtpCmd implements Runnable {
	public static final String message = "TEMPLATE!!"; 
	
	public CmdPWD(SessionThread sessionThread, String input) {
		super(sessionThread, CmdPWD.class.toString());
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "PWD executing");
		sessionThread.writeString("257 \"" +
							      sessionThread.getPrefix().toString() +
							      "\"\r\n");
		myLog.l(Log.DEBUG, "PWD complete");
	}

}
