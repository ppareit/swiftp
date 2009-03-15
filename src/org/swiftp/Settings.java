package org.swiftp;

public class Settings {
	protected static int inputBufferSize = 256;

	public static int getInputBufferSize() {
		return inputBufferSize;
	}

	public static void setInputBufferSize(int inputBufferSize) {
		Settings.inputBufferSize = inputBufferSize;
	}
}
