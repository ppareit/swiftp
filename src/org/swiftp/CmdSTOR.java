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
		File storeFile = inputPathToChrootedFile(sessionThread.getPrefix(), param);
		
		String errString = null;
		FileOutputStream out = null;
		//DedicatedWriter dedicatedWriter = null;
		//int origPriority = Thread.currentThread().getPriority();
		//myLog.l(Log.DEBUG, "STOR original priority: " + origPriority);
		storing: {
			// Get a normalized absolute path for the desired file
			if(violatesChroot(storeFile)) {
				errString = "550 Invalid name or chroot violation\r\n";
				break storing;
			}
			if(storeFile.isDirectory()) {
				errString = "451 Can't overwrite a directory\r\n";
				break storing;
			}

			try {
				if(storeFile.exists()) {
					if(!storeFile.delete()) {
						errString = "451 Couldn't truncate file\r\n";
						break storing;
					}
				}
				out = new FileOutputStream(storeFile, false); // don't append
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
			//dedicatedWriter = new DedicatedWriter(out);
			//dedicatedWriter.start();  // start the writer thread executing
			myLog.l(Log.DEBUG, "Started DedicatedWriter");
			int numRead;
			//Thread.currentThread().setPriority(Thread.MAX_PRIORITY);
			//int newPriority = Thread.currentThread().getPriority();
			//myLog.l(Log.DEBUG, "New STOR prio: " + newPriority);
			while(true) {
				/*if(dedicatedWriter.checkErrorFlag()) {
					errString = "451 File IO problem\r\n";
					break storing;
				}*/
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
						//myLog.l(Log.DEBUG, "Enqueueing buffer of " + numRead);
						//dedicatedWriter.enqueueBuffer(buffer, numRead);
						if(sessionThread.isBinaryMode()) {
							out.write(buffer, 0, numRead);
						} else {
							// ASCII mode, substitute \r\n to \n
							int startPos=0, endPos;
							for(endPos = 0; endPos < numRead; endPos++ ) {
								if(buffer[endPos] == '\r') {
									// Our hacky method is to drop all \r
									out.write(buffer, startPos, endPos-startPos);
									startPos = endPos+1;
								}
							}
							// Write last part of buffer as long as there was something
							// left after handling the last \r
							if(startPos < numRead) {
								out.write(buffer, startPos, endPos-startPos);
							} 
						}
					} catch (IOException e) {
						errString = "451 File buffer queue problem\r\n";
						break storing;
					}
					break;
				}
			}
		}
//		// Clean up the dedicated writer thread
//		if(dedicatedWriter != null) {
//			dedicatedWriter.exit();  // set its exit flag
//			dedicatedWriter.interrupt(); // make sure it wakes up to process the flag
//		}
		//Thread.currentThread().setPriority(origPriority);
		try {
//			if(dedicatedWriter != null) {
//				dedicatedWriter.exit();
//			}
			if(out != null) {
				out.close();
			}
		} catch (IOException e) {}
		
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
