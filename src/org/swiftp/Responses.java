package org.swiftp;

/**
 * This class is a central location for responses that might be sent 
 * in response to several different commands. 
 *
 */
public class Responses {
	public static final byte[] welcomeMsg =
		// "220 SwiFTP ready\r\n" sent upon login
		{'2','2','0',' ','S','w','i','F','T','P',' ',
		 'r','e','a','d','y','\r','\n'};
	public static final byte[] unrecognizedCmdMsg =
		// "500 Command not implemented" when cmd not recognized
		{'5','0','2',' ','C','o','m','m','a','n','d',' ','n','o','t',' ',
		 'i','m','p','l','e','m','e','n','t','e','d','\r','\n'};
	
}
