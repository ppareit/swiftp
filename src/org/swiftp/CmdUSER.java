package org.swiftp;

import android.util.Log;

public class CmdUSER extends FtpCmd implements Runnable {
	protected String input;
	
	public CmdUSER(SessionThread sessionThread, String input) {
		super(sessionThread, CmdUSER.class.toString());
		this.input = input;
		
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "USER executing");
		String username = FtpCmd.getParameter(input);
		username = username.toLowerCase();
		if(!username.toLowerCase().equals("anonymous")) {
			sessionThread.writeString("530 Must use \"anonymous\" user\r\n");
			return;
		}
		sessionThread.writeString("331 Send password\r\n");
		sessionThread.account.setUsername(username);
	}

}
