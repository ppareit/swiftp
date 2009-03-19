package org.swiftp;

public class Settings {
	protected static int inputBufferSize = 256;
	protected static boolean allowOverwrite = false;
	protected static int dataChunkSize = 8192;  // do file I/O in 8k chunks 
	

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
}
