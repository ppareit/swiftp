package org.swiftp;

import java.io.File;
import java.io.IOException;

import android.util.Log;

public class CmdRNFR extends FtpCmd implements Runnable {
	protected String input;

	public CmdRNFR(SessionThread sessionThread, String input) {
		super(sessionThread, CmdRNFR.class.toString());
		this.input = input;
	}
	
	public void run() {
		String param = getParameter(input);
		String errString = null;
		File file = null;
		running: {
			try {
				file = new File(param).getCanonicalFile().getAbsoluteFile();
			} catch (IOException e) {
				sessionThread.writeString("450 Invalid filename\r\n");
				errString = "Couldn't construct File object";
				break running;
			}
			
			if(!file.exists()) {
				errString = "450 Cannot rename nonexistent file\r\n";
			}
		}
		if(errString != null) {
			sessionThread.writeString(errString);
			myLog.l(Log.INFO, "RNFR failed: " + errString.trim());
			sessionThread.setRenameFrom(null);
		} else {
			sessionThread.writeString("350 Filename noted, now send RNTO\r\n");
			sessionThread.setRenameFrom(file);
		}
	}
}
