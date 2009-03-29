package org.swiftp;

import android.util.Log;

public class MyLog {
	protected String tag;
	
	public MyLog(String tag) {
		this.tag = tag;
	}
	public void l(int level, String str) {
		str = str.trim();
		if(level >= Defaults.getConsoleLogLevel()) {
			Log.println(level,tag, str);
		}
		if(level >= Defaults.getUiLogLevel()) {
			FTPServerService.log(level, str);
		}
	}
}
