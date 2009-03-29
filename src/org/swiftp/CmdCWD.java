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

import android.util.Log;

public class CmdCWD extends FtpCmd implements Runnable {
	protected String input;
	
	public CmdCWD(SessionThread sessionThread, String input) {
		super(sessionThread, CmdCWD.class.toString());
		this.input = input;
	}
	
	public void run() {
		myLog.l(Log.DEBUG, "CWD executing");
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
		myLog.l(Log.DEBUG, "CWD complete");
	}
}
