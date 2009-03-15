package org.swiftp;

import java.io.File;
import java.io.IOException;

import android.util.Log;

public class CmdCWD extends FtpCmd implements Runnable {
	protected String input;
	
	public CmdCWD(SessionThread sessionThread, String input) {
		super(sessionThread, "CWD");
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.INFO, "CWD executing");
		String param = getParameter(input);
		File newPrefix;
		if(param.charAt(0) == File.separatorChar) {
			// The user has given an absolute path
			newPrefix = new File(param);
		} else {
			newPrefix = new File(sessionThread.getPrefix(), param);
		}
		try {
			newPrefix = newPrefix.getCanonicalFile();
			if(newPrefix.canRead()) {
				sessionThread.setPrefix(newPrefix);
				sessionThread.writeString("250 CWD successful\r\n");
			} else {
				sessionThread.writeString("550 No read permission\r\n");
			}
		} catch(IOException e) {
			sessionThread.writeString("550 Invalid path\r\n");
		}
	}

}
