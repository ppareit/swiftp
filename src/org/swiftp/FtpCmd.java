package org.swiftp;


import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import android.util.Log;

public abstract class FtpCmd implements Runnable {
	protected SessionThread sessionThread;
	protected MyLog myLog;
	protected static MyLog staticLog = new MyLog("FtpCmd");
	
	protected static CmdMap[] cmdClasses = {
			new CmdMap("SYST", CmdSYST.class),
			new CmdMap("USER", CmdUSER.class),
			new CmdMap("PASS", CmdPASS.class),
			new CmdMap("TYPE", CmdTYPE.class),
			new CmdMap("CWD",  CmdCWD.class),
			new CmdMap("PWD",  CmdPWD.class),
			new CmdMap("LIST", CmdLIST.class),
			new CmdMap("PASV", CmdPASV.class)
	};
	
	public FtpCmd(SessionThread sessionThread, String logName) {
		this.sessionThread = sessionThread;
		myLog = new MyLog(logName);
	}
	
	abstract public void run();
	
	/*protected static byte[][] tokenize(byte[] bytes, int len) {
		List<byte[]> tokenList = new ArrayList<byte[]>();
		// We use the maximum input line size also as the maximum input
		// token size
		int maxTokenSize = Settings.getInputBufferSize();
		
		if(bytes[0] == ' ') {
			return null;
		}
		for(int i=0; i<len; i++) {
			byte[] curToken = new byte[maxTokenSize];
			int j = 0;
			while(i<len) {
				byte cur = bytes[i];
				switch
				curToken[j] = bytes[i];
				i++;
			}
			// If there was a group of spaces and we found just the first
			// one, consume the rest of them now
			while(bytes[i] == ' ' && i<len) {
				i++;
			}
			if(j!=0) {
				tokenList.add(curToken);
			}
		}
		staticLog.l(Log.DEBUG, "Tokens parsed (len " + len + "):");
		for(int i=0; i<tokenList.size(); i++) {
			staticLog.l(Log.DEBUG, "Token " + i + ": " + tokenList.get(i));
		}
		if(tokenList.size() == 0) {
			return null;
		}
		return (byte[][])(tokenList.toArray());
	}*/
	
	protected static void dispatchCommand(SessionThread session, 
	                                      String inputString) {
		String[] strings = inputString.split(" ");
		if(strings == null) {
			// There was some egregious sort of parsing error
			staticLog.l(Log.INFO, "Command parse error");
			session.writeBytes(Responses.unrecognizedCmdMsg);
			return;
		}
		if(strings.length < 1) {
			staticLog.l(Log.INFO, "No strings parsed");
			session.writeBytes(Responses.unrecognizedCmdMsg);
			return;
		}
		String verb = strings[0];
		if(verb.length() < 1) {
			staticLog.l(Log.INFO, "Invalid command verb");
			session.writeBytes(Responses.unrecognizedCmdMsg);
			return;
		}
		FtpCmd cmdInstance = null;
		verb = verb.trim();
		verb = verb.toUpperCase();
		for(int i=0; i<cmdClasses.length; i++) {
			
			if(cmdClasses[i].getName().equals(verb)) {
				// We found the correct command. We retrieve the corresponding
				// Class object, get the Constructor object for that Class, and 
				// and use that Constructor to instantiate the correct FtpCmd 
				// subclass. Yes, I'm serious.
				Constructor<? extends FtpCmd> constructor; 
				try {
					constructor = cmdClasses[i].getCommand().getConstructor(
							new Class[] {SessionThread.class, String.class});
				} catch (NoSuchMethodException e) {
					staticLog.l(Log.ERROR, "FtpCmd subclass lacks expected " +
							           "constructor ");
					return;
				}
				try {
					cmdInstance = constructor.newInstance(
							new Object[] {session, inputString});
				} catch(Exception e) {
					staticLog.l(Log.ERROR, 
							"Instance creation error on FtpCmd");
					return;
				}
			}
		}
		if(cmdInstance == null) {
			// If we couldn't find a matching command, 
			session.writeBytes(Responses.unrecognizedCmdMsg);
			return;
		} else {
			cmdInstance.run();
		}
	}
		
	/**
	 * An FTP parameter is that part of the input string that occurs
	 * after the first space, including any subsequent spaces. Also,
	 * we want to chop off the trailing '\r\n', if present.
	 */
	static public String getParameter(String input) {
		int firstSpacePosition = input.indexOf(' ');
		if(firstSpacePosition == -1) {
			return "";
		}
		String retString = input.substring(firstSpacePosition+1);
		
		// Remove trailing whitespace
		// todo: trailing whitespace may be significant, just remove \r\n
		retString = retString.replaceAll("\\s+$", "");
		
		staticLog.l(Log.DEBUG, "Parsed argument: " + retString);
		return retString; 
	}

}
