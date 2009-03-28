package org.swiftp;

import android.util.Log;

public class CmdPASS extends FtpCmd implements Runnable {
	
	public CmdPASS(SessionThread sessionThread, String input) {
		// We can just discard the password for now. We're just
		// following the expected dialogue, we're going to allow
		// access in any case.
		super(sessionThread, CmdPASS.class.toString());
	}
	
	public void run() {
		// User must have already executed a USER command to
		// populate the Account object's username
		myLog.l(Log.DEBUG, "Executing PASS");
		if(sessionThread.account.getUsername() == null) {
			sessionThread.writeString("503 Must send USER first\r\n");
			return;
		}
		sessionThread.writeString("230 Access granted\r\n");
		sessionThread.setAuthenticated(true);
	}

}
