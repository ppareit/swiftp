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
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import android.util.Log;

public class CmdSTOR extends FtpCmd implements Runnable {
	//public static final String message = "TEMPLATE!!";
	protected String input;
	
	public CmdSTOR(SessionThread sessionThread, String input) {
		super(sessionThread, CmdSTOR.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "STOR executing");
		String param = getParameter(input);
		File storeFile;
		if(param.charAt(0) == '/') {
			// The STOR contained an absolute path
			storeFile = new File(param);
		} else {
			// The STOR contained a relative path
			storeFile = new File(sessionThread.getPrefix(), param); 
		}
		String errString = null;
		
		storing: {
			if(storeFile.exists()) {
				if(!Defaults.isAllowOverwrite()) {
					errString = "451 Server settings prohibit overwrite\r\n";
					myLog.l(Log.INFO, "Prevented overwrite");
					break storing;
				}
			}
			// Get a normalized absolute path for the desired file
			String fileName;
			try {
				fileName = storeFile.getCanonicalFile().getAbsolutePath();
			} catch (IOException e) {
				errString = "450 Invalid file name\r\n";
				myLog.l(Log.INFO, "Filename problem");
				break storing;
			}
			FileOutputStream out;
			try {
				out = new FileOutputStream(fileName);
			} catch(FileNotFoundException e) {
				errString = "451 Couldn't open file for writing\r\n";
				break storing;
			}
			switch(sessionThread.initDataSocket()) {
			case 1:  // successfully opened socket
				break;
			case 2:
				errString = "450 Must use PASV mode\r\n";
				break storing;
			case 0:
				errString = "425 Couldn't open data socket\r\n";
				break storing;
			}
			myLog.l(Log.DEBUG, "Data socket ready");
			sessionThread.writeString("150 Data socket ready\r\n");
			byte[] buffer = new byte[Defaults.getDataChunkSize()];
			int numRead;
			while(true) {
				switch(numRead = sessionThread.receiveFromDataSocket(buffer)) {
				case -1:
					myLog.l(Log.DEBUG, "Returned from final read");
					// We're finished reading
					break storing; 
				case 0: 
					errString = "426 Couldn't receive data\r\n";
					break storing;
				case -2:
					errString = "425 Could not connect data socket\r\n";
					break storing;
				default:
					try {
						out.write(buffer, 0, numRead);
					} catch (IOException e) {
						errString = "451 File IO problem\r\n";
						break storing;
					}
					break;
				}
			}
		}
		if(errString != null) {
			myLog.l(Log.INFO, "STOR error: " + errString.trim());
			sessionThread.writeString(errString);
		} else {
			sessionThread.writeString("226 Transmission complete\r\n");
		}
		sessionThread.closeDataSocket();
		myLog.l(Log.DEBUG, "STOR finished");
	}

}
