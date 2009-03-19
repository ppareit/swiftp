package org.swiftp;

import android.util.Log;

public class CmdSYST extends FtpCmd implements Runnable {
	// This is considered a safe response to the SYST command, see
	// http://cr.yp.to/ftp/syst.html
	public static final String response = "215 UNIX Type: L8\r\n";
	
	public CmdSYST(SessionThread sessionThread, String input) {
		super(sessionThread, CmdSYST.class.toString());
	}
	
	
	public void run() {
		myLog.l(Log.DEBUG, "SYST executing");
		sessionThread.writeString(response);
		myLog.l(Log.DEBUG, "SYST finished");
	}
}
