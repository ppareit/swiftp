package org.swiftp;

import android.util.Log;

public class Settings {
	protected static int inputBufferSize = 256;
	protected static boolean allowOverwrite = false;
	protected static int dataChunkSize = 8192;  // do file I/O in 8k chunks 
	protected static int sessionMonitorScrollBack = 10;
	protected static int serverLogScrollBack = 10;
	protected static int uiLogLevel = Log.INFO;
	
	public static int getUiLogLevel() {
		return uiLogLevel;
	}

	public static void setUiLogLevel(int uiLogLevel) {
		Settings.uiLogLevel = uiLogLevel;
	}

	public static int getInputBufferSize() {
		return inputBufferSize;
	}

	public static void setInputBufferSize(int inputBufferSize) {
		Settings.inputBufferSize = inputBufferSize;
	}

	public static boolean isAllowOverwrite() {
		return allowOverwrite;
	}

	public static void setAllowOverwrite(boolean allowOverwrite) {
		Settings.allowOverwrite = allowOverwrite;
	}

	public static int getDataChunkSize() {
		return dataChunkSize;
	}

	public static void setDataChunkSize(int dataChunkSize) {
		Settings.dataChunkSize = dataChunkSize;
	}

	public static int getSessionMonitorScrollBack() {
		return sessionMonitorScrollBack;
	}

	public static void setSessionMonitorScrollBack(
			int sessionMonitorScrollBack) 
	{
		Settings.sessionMonitorScrollBack = sessionMonitorScrollBack;
	}

	public static int getServerLogScrollBack() {
		return serverLogScrollBack;
	}

	public static void setLogScrollBack(int serverLogScrollBack) {
		Settings.serverLogScrollBack = serverLogScrollBack;
	}
	
	
}
