package org.swiftp;

import android.util.Log;

public class MyLog {
	protected String tag;
	
	public MyLog(String tag) {
		this.tag = tag;
	}
	public void l(int level, String str) {
		Log.println(level,tag, str);
		FTPServerService.log(level, str);
	}
}
