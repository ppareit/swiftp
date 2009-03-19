/**
 * 
 */
package org.swiftp;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.List;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.WifiLock;
import android.os.IBinder;
import android.util.Log;

/**
 * @author david
 *
 */
public class FTPServerService extends Service implements Runnable {
	/**
	 * 
	 */
	protected static Thread serverThread = null;
	protected boolean shouldExit = false;
	protected MyLog myLog = new MyLog(getClass().getName());
	
	protected static final int PORT = 2121;
	protected static final int BACKLOG = 21;
	protected static final int MAX_SESSIONS = 5;
	
	protected ServerSocketChannel mainSocket;
	protected static WifiLock wifiLock = null;
	
	// The server thread will check this often to look for incoming 
	// connections. We are forced to use non-blocking accept() and polling
	// because we cannot wait forever in accept() if we want to be able
	// to receive an exit signal and cleanly exit.
	public static final int WAKE_INTERVAL_MS = 1000; // milliseconds  
	
	public FTPServerService() {
		// Set the application-wide context global, if not already set
		Context myContext = Globals.getContext();
		if(myContext == null) {
			myContext = getApplicationContext();
			if(myContext != null) {
				Globals.setContext(myContext);
			}
		}
		return;
	}

	public IBinder onBind(Intent intent) {
		// We don't implement this functionality, so ignore it
		return null;
	}
	
	public void onCreate() {
		// Don't do anything until onStart()
		myLog.l(Log.INFO, "SwiFTP server created");
		return;
	}
	
	public void onStart(Intent intent, int startId ){
		super.onStart(intent, startId);
		
		shouldExit = false;
		if(serverThread != null) {
			myLog.l(Log.ERROR, "Won't start, server thread exists");
			return;
		}
		myLog.l(Log.DEBUG, "Creating server thread");
		serverThread = new Thread(this);
		serverThread.start();
		
		// prevent sleeping as long as the service is running
		if(wifiLock == null) {
			WifiManager manager = (WifiManager) getSystemService(Context.WIFI_SERVICE);
			wifiLock = manager.createWifiLock("SwiFTP");
			wifiLock.setReferenceCounted(true);
		}
		wifiLock.acquire();

		// todo: we should broadcast an intent to inform anyone who cares
	}
	
	public static boolean isRunning() {
		// return true if and only if a server Thread is running
		return (serverThread != null);
	}
	
	public void onDestroy() {
		myLog.l(Log.INFO, "Stopping");
		shouldExit = true;
		if(serverThread == null) {
			myLog.l(Log.WARN, "Stopping with null serverThread");
			return;
		}
		serverThread.interrupt();
		try {
			serverThread.join(1000);  // wait 1 sec for server thread to finish
		} catch (InterruptedException e) {}
		if(serverThread.isAlive()) {
			myLog.l(Log.ERROR, "Server thread failed to exit");
			// it may still exit eventually if we just leave the
			// shouldExit flag set
		} else {
			serverThread = null;
		}
		try {
			myLog.l(Log.INFO, "Closing mainSocket");
			mainSocket.close();
		} catch (IOException e) {}
		UiUpdater.updateClients();
		if(wifiLock != null) {
			wifiLock.release();
		}
		// todo: we should broadcast an intent to inform anyone who cares
	}
	
	public void run() {
		// The UI will want to check the server status to update its 
		// start/stop server button
		UiUpdater.updateClients();
		List<SessionThread> sessionThreads = new ArrayList<SessionThread>();
				
		// Open up a non-blocking server socket
		// thanks to Patrick Chan's Java Developers Almanac for this idea
		/*Context appContext = getApplicationContext();
		ConnectivityManager connMgr = (ConnectivityManager)
			appContext.getSystemService(Context.CONNECTIVITY_SERVICE);*/
		myLog.l(Log.DEBUG, "Server thread running");
		
		try {
			mainSocket = ServerSocketChannel.open();
			mainSocket.configureBlocking(false);
			String wifiIp = getWifiIpAsString();
			if(wifiIp == null) { 
				throw new IOException("Wifi not enabled");
			}
			InetAddress inetAddr = InetAddress.getByName(wifiIp);
			mainSocket.socket().bind(new InetSocketAddress(inetAddr, PORT));
		} catch (IOException e) {
			myLog.l(Log.ERROR, "Error opening port");
			cleanupAndStopService();
			return;
		}
		myLog.l(Log.DEBUG, "Port opened");
		
		// The main loop: wait for inbound TCP connections and dispatch
		// them to handler threads. If any session threads have finished,
		// then remove them from our sessionThreads list.
		while(!shouldExit) {
			SocketChannel clientSocket;
			try {
				// Handle one of more incoming connection requests
				while((clientSocket = mainSocket.accept()) != null) {
					// If the accept was successful, spawn a new session
					myLog.l(Log.INFO, "Spawned session thread");
					SessionThread newSession = new SessionThread(clientSocket, this);
					sessionThreads.add(newSession);
					newSession.start();
				}
			} catch(IOException e) {
				myLog.l(Log.ERROR, "Error in socket accept");
				myLog.l(Log.ERROR, e.toString());
				cleanupAndStopService();
				return;
			}
			try {
				// todo: think about using ServerSocket, and just closing
				// the main socket to send an exit signal
				Thread.sleep(WAKE_INTERVAL_MS);
			} catch(InterruptedException e) {
				myLog.l(Log.DEBUG, "Thread interrupted");
			}
			// Look for finished session threads and stop tracking them in
			// the sessionThreads list
			
			// Since we're not allowed to modify the list while iterating over
			// it, we construct a list in toBeRemoved of threads to remove
			// later from the sessionThreads list.
			List <SessionThread> toBeRemoved = new ArrayList<SessionThread>();
			for(SessionThread sessionThread : sessionThreads) {
				if(!sessionThread.isAlive()) {
					myLog.l(Log.DEBUG, "Cleaning up finished session...");
					try {
						sessionThread.join();
						myLog.l(Log.DEBUG, "Thread joined");
						toBeRemoved.add(sessionThread);
						sessionThread.closeSocket(); // make sure socket closed
					} catch (InterruptedException e) {
						myLog.l(Log.DEBUG, "Interrupted while joining");
						// We will try again in the next loop iteration
					}
				}
			}
			for(SessionThread removeThread : toBeRemoved) {
				sessionThreads.remove(removeThread);
			}
		}
			
		myLog.l(Log.DEBUG, "Exiting cleanly");
		shouldExit = false; // we handled the exit flag, so reset it
	}
		
	public void cleanupAndStopService() {
		// Call the Android Service shutdown function
		Context context = getApplicationContext();
		Intent intent = new Intent(context,	FTPServerService.class);
		context.stopService(intent);
	}
	
	public void errorShutdown() {
		myLog.l(Log.ERROR, "Service errorShutdown() called");
		cleanupAndStopService();
	}

	/**
	 * Gets the IP address of the wifi connection.
	 * @return The integer IP address if wifi enabled, or -1 if not.
	 */
	public static int getWifiIpAsInt() {
		Context myContext = Globals.getContext();
		if(myContext == null) {
			throw new NullPointerException("Global context is null");
		}
		WifiManager wifiMgr = (WifiManager)myContext
		                        .getSystemService(Context.WIFI_SERVICE);
		if(wifiMgr.getWifiState() == WifiManager.WIFI_STATE_ENABLED) {
			return wifiMgr.getConnectionInfo().getIpAddress();
		} else {
			return -1;
		}
	}
	
	public static String getWifiIpAsString() {
		int addr = getWifiIpAsInt();
		if(addr > 0) {
			StringBuffer buf = new StringBuffer();
			buf.append(addr & 0xff).append('.').
	        append((addr >>>= 8) & 0xff).append('.').
	        append((addr >>>= 8) & 0xff).append('.').
	        append((addr >>>= 8) & 0xff);
			return buf.toString();
		} else {
			return null;
		}
	}
}
