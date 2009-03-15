package org.swiftp;

import android.content.Context;

public class Globals {
	private static Context context;

	public static Context getContext() {
		return context;
	}

	public static void setContext(Context context) {
		if(context != null) { 
			Globals.context = context;
		}
	}
	
}
