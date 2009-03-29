/*
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
*/

package org.swiftp;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import android.util.Log;

public class CmdLIST extends FtpCmd implements Runnable {
	//public static final String message = "LIST";
	protected static MyLog staticLog = new MyLog(CmdLIST.class.toString());
	protected String input;
	// The approximate number of milliseconds in 6 months
	public final static long MS_IN_SIX_MONTHS = 6 * 30 * 24 * 60 * 60 * 1000; 
	
	public CmdLIST(SessionThread sessionThread, String input) {
		super(sessionThread, CmdLIST.class.toString());
		this.input = input; 
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "LIST executing");

		String param = getParameter(input);
		File fileToList = null;
		if(param.length() > 0) {
			// the user specified "LIST <argument>"
			if(param.charAt(0) == '/') {
				// The LIST parameter is an absolute path
				fileToList = new File(param);
			} else if (param.charAt(0) == '-') {
				// The parameter is some options to ls, which we ignore
				fileToList = sessionThread.getPrefix();
			} else {
				// The LIST parameter is a relative path,
				// so append it to the existing path prefix
				fileToList = new File(sessionThread.getPrefix(), param);
			}
		} else {
			// The user did not give a parameter to LIST. So we just
			// use the current directory for the session.
			fileToList = sessionThread.getPrefix();
		}
		// Normalize the path representation
		try {
			fileToList = fileToList.getCanonicalFile();
		} catch (IOException e) {
			myLog.l(Log.INFO, "Error getting canonical path");
			sessionThread.writeString("451 Path problem\r\n");
			return;
		}
		myLog.l(Log.DEBUG, "Listing name: " + fileToList.toString());
		StringBuilder response = new StringBuilder();
		
		if(fileToList.isDirectory()) {
			myLog.l(Log.DEBUG, "Listing directory");
			// Get a listing of all files and directories in the path
			File[] entries = fileToList.listFiles();
			myLog.l(Log.DEBUG, "Dir len " + entries.length);
			for(File entry : entries) {
				myLog.l(Log.DEBUG, "Handling dentry");
				String curLine = makeLsString(entry);
				if(curLine != null) {
					response.append(curLine + "\r\n");
				}
			}
		} else {
			myLog.l(Log.DEBUG, "Listing file");
			// The given path is a file and not a directory
			response.append(makeLsString(fileToList));
			response.append("\r\n");
		}
		
		boolean err = false;
		String errString = null;
		switch(sessionThread.initDataSocket()) {
		case 1: // success
			break;
		case 2:
			err = true;
			errString = "425 Must use PASV mode\r\n";
			break;
		case 0:
		default:
			err = true;
			errString = "425 Error opening data socket\r\n";
			break;
		}
		if(!err) {
			sessionThread.writeString("150 Beginning transmission\r\n");
			if(!sessionThread.sendViaDataSocket(response.toString())) {
				errString = "426 Data socket or network error\r\n";
			} else {
				myLog.l(Log.DEBUG, "sendViaDataSocket success");
			}
		}
		sessionThread.closeDataSocket();
		if(err) {
			sessionThread.writeString(errString);
		} else {
			sessionThread.writeString("226 Data transmission OK\r\n");
		}
		myLog.l(Log.DEBUG, "LIST complete");
	}
	
	private static String makeLsString(File file) {
		StringBuilder response = new StringBuilder();
		
		if(!file.exists()) {
			staticLog.l(Log.ERROR, "makeLsString had nonexistent file");
			return null;
		}

		// See Daniel Bernstein's explanation of /bin/ls format at:
		// http://cr.yp.to/ftp/list/binls.html
		// This stuff is almost entirely based on his recommendations.
		
		String lastNamePart = file.getName();
		// Many clients can't handle files containing these symbols
		if(lastNamePart.contains("*") || 
		   lastNamePart.contains("/"))
		{
			staticLog.l(Log.INFO, "Filename omitted due to disallowed character");
			return null;
		}
				
		
		if(file.isDirectory()) {
			response.append("drwxr-xr-x 1 owner group");
		} else {
			// todo: think about special files, symlinks, devices
			response.append("-rw-r--r-- 1 owner group");
		}
		
		// The next field is a 13-byte right-justified space-padded file size
		long fileSize = file.length();
		String sizeString = new Long(fileSize).toString();
		int padSpaces = 13 - sizeString.length();
		while(padSpaces-- > 0) {
			response.append(' ');
		}
		response.append(sizeString);
		
		// The format of the timestamp varies depending on whether the mtime
		// is 6 months old
		long mTime = file.lastModified();
		SimpleDateFormat format;
		if(System.currentTimeMillis() - mTime > MS_IN_SIX_MONTHS) {
			// The mtime is less than 6 months ago
			format = new SimpleDateFormat(" MMM dd HH:mm ");
		} else {
			// The mtime is more than 6 months ago
			format = new SimpleDateFormat(" MMM dd  yyyy ");
		}
		response.append(format.format(new Date(file.lastModified())));
		response.append(lastNamePart);
		return response.toString();
	}

}
