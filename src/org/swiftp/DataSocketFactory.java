package org.swiftp;

import java.net.InetAddress;
import java.net.Socket;


abstract public class DataSocketFactory {
	
	/**
	 * A DataSocketFactory hides the implementation of the opening and closing
	 * of the data sockets which are used to transmit directory listings and
	 * file contents. This is necessary because normal FTP data sockets are
	 * opened and closed very differently from the abnormal sort of data sockets
	 * we use in conjunction with our cloud proxy system.
	 */
	protected MyLog myLog = new MyLog(getClass().getName());
	
	/**
	 * When SwiFTP receives a PORT command, this will be called. Subclasses should
	 * perform whatever initialization is necessary.
	 * @return Whether the necessary actions completed successfully
	 */
	abstract public boolean onPort(InetAddress dest, int port);
	
	/**
	 * When SwiFTP receives a PASV command, this will be called. Subclasses should
	 * perform whatever initialization is necessary.
	 * @return Whether the necessary actions completed successfully
	 */
	abstract public boolean onPasv();

	/**
	 * When it's time for data transfer to begin, the SessionThread will call this
	 * method to perform any necessary actions to prepare the Socket for use and
	 * return it in a state that's ready for reading or writing.
	 * @return The opened Socket
	 */
	abstract public Socket onTransfer();
	
	abstract public int getPortNumber();
	
	/**
	 * Sometimes we'll need to know the IP address at which we can be contacted. For
	 * instance, the response to a PASV command will be the IP and port that the
	 * client should use to connect it's data socket.
	 */
	abstract public String getPasvIp();
}

