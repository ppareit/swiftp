package org.swiftp;

import java.io.File;
import java.io.IOException;

import android.util.Log;

public class CmdMKD extends FtpCmd implements Runnable {
	String input;
	
	public CmdMKD(SessionThread sessionThread, String input) {
		super(sessionThread, CmdMKD.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "MKD executing");
		String param = getParameter(input);
		File toCreate;
		String errString = null;
		mainblock: {
			try {
				toCreate = new File(param)
				            .getCanonicalFile().getAbsoluteFile();
			} catch(IOException e) {
				errString = "550 Invalid name\r\n";
				break mainblock;
			}
			if(!toCreate.mkdir()) {
				errString = "550 Error during creation operation\r\n";
				break mainblock;
			}
		}
		if(errString != null) {
			sessionThread.writeString(errString);
			myLog.l(Log.INFO, "MKD error: " + errString.trim());
		} else {
			sessionThread.writeString("250 Directory created\r\n");
		}
		myLog.l(Log.INFO, "MKD complete");
	}

}
