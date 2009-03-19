package org.swiftp;

import android.util.Log;

public class CmdTYPE extends FtpCmd implements Runnable {
	String input;
	
	public CmdTYPE(SessionThread sessionThread, String input) {
		super(sessionThread, CmdTYPE.class.toString());
		this.input = input;
	}
	
	public void run() {
		String output;
		myLog.l(Log.INFO, "TYPE executing");
		String param = getParameter(input);
		if(param.equals("I") || param.equals("L 8")) {
			output = "200 Binary type set\r\n";
			sessionThread.setBinaryMode(true);
		} else if (param.equals("A") || param.equals("A N")) {
			output = "200 ASCII type set\r\n";
			sessionThread.setBinaryMode(false);
		} else {
			output = "503 Malformed TYPE command\r\n";
		}
		sessionThread.writeString(output);
		myLog.l(Log.INFO, "TYPE complete");
	}

}
