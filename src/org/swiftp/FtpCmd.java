package org.swiftp;


import java.lang.reflect.Constructor;

import android.util.Log;

public abstract class FtpCmd implements Runnable {
	protected SessionThread sessionThread;
	protected MyLog myLog;
	protected static MyLog staticLog = new MyLog(FtpCmd.class.toString());
	
	protected static CmdMap[] cmdClasses = {
			new CmdMap("SYST", CmdSYST.class),
			new CmdMap("USER", CmdUSER.class),
			new CmdMap("PASS", CmdPASS.class),
			new CmdMap("TYPE", CmdTYPE.class),
			new CmdMap("CWD",  CmdCWD.class),
			new CmdMap("PWD",  CmdPWD.class),
			new CmdMap("LIST", CmdLIST.class),
			new CmdMap("PASV", CmdPASV.class),
			new CmdMap("RETR", CmdRETR.class),
			new CmdMap("NOOP", CmdNOOP.class),
			new CmdMap("STOR", CmdSTOR.class),
			new CmdMap("DELE", CmdDELE.class),
			new CmdMap("RNFR", CmdRNFR.class),
			new CmdMap("RNTO", CmdRNTO.class),
			new CmdMap("RMD",  CmdRMD.class),
			new CmdMap("MKD",  CmdMKD.class),
			new CmdMap("PORT", CmdPORT.class)
	};
	
	public FtpCmd(SessionThread sessionThread, String logName) {
		this.sessionThread = sessionThread;
		myLog = new MyLog(logName);
	}
	
	abstract public void run();
	
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
			staticLog.l(Log.INFO, "Ignoring unrecognized FTP verb: " + verb);
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
