package org.swiftp;

import java.io.File;
import java.io.IOException;

import android.util.Log;

public class CmdRMD extends FtpCmd implements Runnable {
	public static final String message = "TEMPLATE!!";
	protected String input;
	
	public CmdRMD(SessionThread sessionThread, String input) {
		super(sessionThread, CmdRMD.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.INFO, "RMD executing");
		String param = getParameter(input);
		File toRemove;
		String errString = null;
		mainblock: {
			if(param.length() < 1) {
				errString = "550 Invalid argument\r\n";
				break mainblock;
			}
			if(param.charAt(0) == '/') {
				toRemove = new File(param);
			} else {
				// The param is a relative path, prepend the working directory
				toRemove = new File(sessionThread.getPrefix(), param);
			}
			try {
				toRemove = toRemove.getCanonicalFile().getAbsoluteFile();
				myLog.l(Log.DEBUG, "toRemove: " + toRemove.getAbsolutePath());

			} catch (IOException e) {
				errString = "550 Invalid directory name\r\n";
				break mainblock;
			}
			if(!toRemove.isDirectory()) {
				errString = "550 Can't RMD a non-directory\r\n";
				break mainblock;
			}
			if(toRemove.equals(new File("/"))) {
				errString = "550 Won't RMD the root directory\r\n";
				break mainblock;
			}
			if(!recursiveDelete(toRemove)) {
				errString = "550 Deletion error, possibly incomplete\r\n";
				break mainblock;
			}
		}
		if(errString != null) {
			sessionThread.writeString(errString);
			myLog.l(Log.INFO, "RMD failed: " + errString.trim());
		} else {
			sessionThread.writeString("250 Removed directory\r\n");
		}
		myLog.l(Log.DEBUG, "RMD finished");
	}
	
	/**
	 * Accepts a file or directory name, and recursively deletes the contents
	 * of that directory and all subdirectories.
	 * @param toDelete
	 * @return Whether the operation completed successfully
	 */
	protected boolean recursiveDelete(File toDelete) {
		if(!toDelete.exists()) {
			return false;
		}
		if(toDelete.isDirectory()) {
			// If any of the recursive operations fail, then we return false
			boolean success = true;
			for(File entry : toDelete.listFiles()) {
				success &= recursiveDelete(entry);
			}
			myLog.l(Log.DEBUG, "Recursively deleted: " + toDelete);
			return success && toDelete.delete();
		} else {
			myLog.l(Log.DEBUG, "RMD deleting file: " + toDelete);
			return toDelete.delete();
		} 
	}
}
