package org.swiftp;

import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;

abstract public class Util {
	static MyLog myLog = new MyLog(Util.class.getName());
	static String getAndroidId() {
		return android.provider.Settings.Secure.ANDROID_ID;
	}
	
	/**
	 * Get the SwiFTP version from the manifest.
	 * @return The version as a String.
	 */
	public static String getVersion() {
		String packageName = Globals.getContext().getPackageName();
		try {
			return Globals.getContext().getPackageManager().getPackageInfo(packageName, 0).versionName;
		} catch ( NameNotFoundException e) {
			myLog.l(Log.ERROR, "NameNotFoundException looking up SwiFTP version");
			return null;
		}
	}
}
