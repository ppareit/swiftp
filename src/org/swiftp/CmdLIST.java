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

		//String param = getParameter(input);
		String errString = null;
		File fileToList = null;
		mainblock: {
			// An FTP "LIST" verb always means list current directory
			fileToList = sessionThread.getPrefix();
	
			myLog.l(Log.DEBUG, "Listing: " + fileToList.toString());
			StringBuilder response = new StringBuilder();
			
			if(fileToList.isDirectory()) {
				myLog.l(Log.DEBUG, "Listing directory");
				// Get a listing of all files and directories in the path
				File[] entries = fileToList.listFiles();
				myLog.l(Log.DEBUG, "Dir len " + entries.length);
				for(File entry : entries) {
					//myLog.l(Log.DEBUG, "Handling dentry");
					String curLine = makeLsString(entry);
					if(curLine != null) {
						response.append(curLine + "\r\n");
					}
				}
			} else {
				// The given path is a file and not a directory
				response.append(makeLsString(fileToList));
				response.append("\r\n");
			}
			
			switch(sessionThread.initDataSocket()) {
			case 1: // success
				myLog.l(Log.DEBUG, "LIST done making socket");
				break;
			case 2:
				myLog.l(Log.DEBUG, "data socket create failure 2");
				errString = "425 Must use PASV mode\r\n";
				break mainblock;
			case 0:
				myLog.l(Log.DEBUG, "data socket create failure 0");
				// no break here, fall through to default case
			default:
				errString = "425 Error opening data socket\r\n";
				break mainblock;
			}
			sessionThread.writeString("150 Beginning transmission\r\n");
			myLog.l(Log.DEBUG, "Sent code 150, sending listing string now");
			if(!sessionThread.sendViaDataSocket(response.toString())) {
				errString = "426 Data socket or network error\r\n";
				myLog.l(Log.DEBUG, "sendViaDataSocket failure");
			} else {
				myLog.l(Log.DEBUG, "sendViaDataSocket success");
			}
		}
		sessionThread.closeDataSocket();
		if(errString != null) {
			sessionThread.writeString(errString);
			myLog.l(Log.DEBUG, "Failed with: " + errString);
		} else {
			sessionThread.writeString("226 Data transmission OK\r\n");
			myLog.l(Log.DEBUG, "List completed OK");
		}
		//myLog.l(Log.DEBUG, "LIST complete");
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
