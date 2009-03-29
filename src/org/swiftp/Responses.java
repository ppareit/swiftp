/*
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
*/

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
