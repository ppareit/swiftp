package org.swiftp;

import java.io.File;

import android.util.Log;

public class CmdDELE extends FtpCmd implements Runnable {
	protected String input; 
	
	public CmdDELE(SessionThread sessionThread, String input) {
		super(sessionThread, CmdDELE.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.INFO, "DELE executing");
		String param = getParameter(input);
		File storeFile;
		if(param.charAt(0) == '/') {
			// The parameter is an absolute path
			storeFile = new File(param);
		} else {
			// The parameter is a relative path
			storeFile = new File(sessionThread.getPrefix(), param); 
		}
		String errString = null;
		if(storeFile.isDirectory()) {
			errString = "550 Can't DELE a directory\r\n";
		} else if(!storeFile.delete()) {
			errString = "450 Error deleting file\r\n";
		}
		
		
		if(errString != null) {
			sessionThread.writeString(errString);
			myLog.l(Log.INFO, "DELE failed: " + errString.trim());
		} else {
			sessionThread.writeString("250 File successfully deleted\r\n");
		}
		myLog.l(Log.INFO, "DELE finished");
	}

}
