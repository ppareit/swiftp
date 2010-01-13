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

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.WifiLock;
import android.os.IBinder;
import android.util.Log;


public class FTPServerService extends Service implements Runnable {
	protected static Thread serverThread = null;
	protected boolean shouldExit = false;
	protected MyLog myLog = new MyLog(getClass().getName());
	protected static MyLog staticLog = 
		new MyLog(FTPServerService.class.getName());
	
	protected static final int BACKLOG = 21;
	protected static final int MAX_SESSIONS = 5;
	
	//protected ServerSocketChannel wifiSocket;
	protected ServerSocket listenSocket;
	protected static WifiLock wifiLock = null;
	
//	protected static InetAddress serverAddress = null;
	
	protected static List<String> sessionMonitor = new ArrayList<String>();
	protected static List<String> serverLog = new ArrayList<String>();
	protected static int uiLogLevel = Defaults.getUiLogLevel();
	
	// The server thread will check this often to look for incoming 
	// connections. We are forced to use non-blocking accept() and polling
	// because we cannot wait forever in accept() if we want to be able
	// to receive an exit signal and cleanly exit.
	public static final int WAKE_INTERVAL_MS = 1000; // milliseconds
	
	protected static int port;
	protected static boolean acceptWifi;
	protected static boolean acceptNet;
	
	private TcpListener wifiListener = null;
	private ProxyConnector proxyConnector = null;
	private List<SessionThread> sessionThreads = new ArrayList<SessionThread>();
	
	private static SharedPreferences settings = null;
	
	NotificationManager notificationMgr = null;
	
	public FTPServerService() {
	}

	public IBinder onBind(Intent intent) {
		// We don't implement this functionality, so ignore it
		return null;
	}
	
	public void onCreate() {
		myLog.l(Log.DEBUG, "SwiFTP server created");
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
	
	public void onStart(Intent intent, int startId ){
		super.onStart(intent, startId);
		
		shouldExit = false;
		int attempts = 10;
		// The previous server thread may still be cleaning up, wait for it
		// to finish.
		while(serverThread != null) {
			myLog.l(Log.WARN, "Won't start, server thread exists");
			if(attempts > 0) {
				attempts--;
				Util.sleepIgnoreInterupt(1000);
			} else {
				myLog.l(Log.ERROR, "Server thread already exists");
				return;
			}
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
		if(serverThread == null) {
			staticLog.l(Log.DEBUG, "Server is not running (null serverThread)");
			return false;
		}
		if(!serverThread.isAlive()) {
			staticLog.l(Log.DEBUG, "serverThread non-null but !isAlive()");
		} else {
			staticLog.l(Log.DEBUG, "Server is alive");
		}
		return true;
	}
	
	public void onDestroy() {
		myLog.l(Log.INFO, "onDestroy() Stopping server");
		shouldExit = true;
		if(serverThread == null) {
			myLog.l(Log.WARN, "Stopping with null serverThread");
			return;
		} else {
			serverThread.interrupt();
			try {
				serverThread.join(10000);  // wait 10 sec for server thread to finish
			} catch (InterruptedException e) {}
			if(serverThread.isAlive()) {
				myLog.l(Log.WARN, "Server thread failed to exit");
				// it may still exit eventually if we just leave the
				// shouldExit flag set
			} else {
				myLog.d("serverThread join()ed ok");
				serverThread = null;
			}
		}
		try {
			if(listenSocket != null) {
				myLog.l(Log.INFO, "Closing listenSocket");
				listenSocket.close();
			}
		} catch (IOException e) {}

		UiUpdater.updateClients();
		if(wifiLock != null) {
			wifiLock.release();
			wifiLock = null;
		}
		clearNotification();
		myLog.d("FTPServerService.onDestroy() finished");
	}
	
	private boolean loadSettings() {
		myLog.l(Log.DEBUG, "Loading settings");
		settings = getSharedPreferences(
				Defaults.getSettingsName(), Defaults.getSettingsMode());
		port = settings.getInt("portNum", Defaults.portNumber);
		if(port == 0) {
			// If port number from settings is invalid, use the default
			port = Defaults.portNumber;
		}
		myLog.l(Log.DEBUG, "Using port " + port);
		
		acceptNet = settings.getBoolean(ConfigureActivity.ACCEPT_NET,
									    Defaults.acceptNet);
		acceptWifi = settings.getBoolean(ConfigureActivity.ACCEPT_WIFI,
										 Defaults.acceptWifi);
		
		// The username, password, and chrootDir are just checked for sanity
		String username = settings.getString(ConfigureActivity.USERNAME, null);
		String password = settings.getString(ConfigureActivity.PASSWORD, null);
		String chrootDir = settings.getString(ConfigureActivity.CHROOTDIR,
				Defaults.chrootDir);
		
		validateBlock: {
			if(username == null || password == null) {
				myLog.l(Log.ERROR, "Username or password is invalid");
				break validateBlock;
			}
			File chrootDirAsFile = new File(chrootDir);
			if(!chrootDirAsFile.isDirectory()) {
				myLog.l(Log.ERROR, "Chroot dir is invalid");
				break validateBlock;
			}
			Globals.setChrootDir(chrootDirAsFile);
			Globals.setUsername(username);
			return true;
		}
		// We reach here if the settings were not sane
		return false;
	}

	// This is old code. We used to get the Wifi IP address from Android, then 
	// listen only on that IP. The new way is to listen on all interfaces.
//	void setupWifiListener() throws IOException {
//		//wifiSocket = ServerSocketChannel.open();
//		//wifiSocket.configureBlocking(false);
//		myLog.l(Log.DEBUG, "About to get wifi IP");
//		int loops = 0;
//		serverAddress = null;
//		while(serverAddress == null) {
//			// If IP address retrieval fails, it may be because DHCP is
//			// still coming up. So we wait one second between attempts,
//			// with a max # of attempts Defaults.getIpRetrievalAttempts().
//			serverAddress = getWifiIp();
//			if(serverAddress != null) {
//				break;
//			}
//			myLog.l(Log.DEBUG, "Wifi IP string was null");
//			loops++;
//			if(loops > Defaults.getIpRetrievalAttempts()) {
//				throw new IOException("IP retrieval failure");	
//			}
//			try {
//				Thread.sleep(1000);
//			} catch(InterruptedException e) {}
//		}
//		myLog.l(Log.DEBUG, "Wifi IP: " + serverAddress.getHostAddress());
//		wifiSocket = new ServerSocket();
//		wifiSocket.bind(new InetSocketAddress(serverAddress, port));
//		// The following line listens on all interfaces
//		// mainSocket.socket().bind(new InetSocketAddress(port));
//		
//	}
	
	// This opens a listening socket on all interfaces. 
	void setupListener() throws IOException {
		listenSocket = new ServerSocket();
		listenSocket.setReuseAddress(true);
		listenSocket.bind(new InetSocketAddress(port));
	}
	
	private void setupNotification() {
		// http://developer.android.com/guide/topics/ui/notifiers/notifications.html
		
		// Get NotificationManager reference
		String ns = Context.NOTIFICATION_SERVICE;
		notificationMgr = (NotificationManager) getSystemService(ns);
		
		// Instantiate a Notification
		int icon = R.drawable.notification;
		CharSequence tickerText = getString(R.string.notif_server_starting);
		long when = System.currentTimeMillis();
		Notification notification = new Notification(icon, tickerText, when);

		// Define Notification's message and Intent
		CharSequence contentTitle = getString(R.string.notif_title);
		CharSequence contentText = getString(R.string.notif_text);
		Intent notificationIntent = new Intent(this, ServerControlActivity.class);
		PendingIntent contentIntent = PendingIntent.getActivity(this, 0, 
				notificationIntent, 0);
		notification.setLatestEventInfo(getApplicationContext(), 
				contentTitle, contentText, contentIntent);
		notification.flags |= Notification.FLAG_ONGOING_EVENT;
		
		// Pass Notification to NotificationManager
		notificationMgr.notify(0, notification);
		
		myLog.d("Notication setup done");
	}
	
	private void clearNotification() {
		if(notificationMgr == null) {
			// Get NotificationManager reference
			String ns = Context.NOTIFICATION_SERVICE;
			notificationMgr = (NotificationManager) getSystemService(ns);
		}
		notificationMgr.cancelAll();
		myLog.d("Cleared notification");
	}
	
	public void run() {
		// The UI will want to check the server status to update its 
		// start/stop server button
		int consecutiveProxyStartFailures = 0;
		long proxyStartMillis = 0;

		UiUpdater.updateClients();
				
		myLog.l(Log.DEBUG, "Server thread running");
		
		// set our members according to user preferences
		if(!loadSettings()) {
			// loadSettings returns false if settings are not sane
			cleanupAndStopService();
			return;
		}
		
		setupNotification();
		
		if(acceptWifi) {
			// If configured to accept connections via wifi, then set up the socket
			try {
				setupListener();
			} catch (IOException e) {
				myLog.l(Log.WARN, "Error opening port, check your network connection.");
//				serverAddress = null;
				cleanupAndStopService();
				return;
			}
		}
		myLog.l(Log.INFO, "SwiFTP server ready");
		
		// We should update the UI now that we have a socket open, so the UI
		// can present the URL
		UiUpdater.updateClients();
		
		while(!shouldExit) {
			if(acceptWifi) {
				if(wifiListener != null) {
					if(!wifiListener.isAlive()) {
						myLog.l(Log.DEBUG, "Joining crashed wifiListener thread");
						try {
							wifiListener.join();
						} catch (InterruptedException e) {}
						wifiListener = null;
					}
				}
				if(wifiListener == null) {
					// Either our wifi listener hasn't been created yet, or has crashed,
					// so spawn it
					wifiListener = new TcpListener(listenSocket, this); 
					wifiListener.start();
				}
			}
			if(acceptNet) {
				if(proxyConnector != null) {
					if(!proxyConnector.isAlive()) {
						myLog.l(Log.DEBUG, "Joining crashed proxy connector");
						try {
							proxyConnector.join();
						} catch (InterruptedException e) {}
						proxyConnector = null;
						long nowMillis = new Date().getTime();
						//myLog.l(Log.DEBUG, "Now:"+nowMillis+" start:"+proxyStartMillis);
						if(nowMillis - proxyStartMillis < 3000) {
							// We assume that if the proxy thread crashed within 3
							// seconds of starting, it was a startup or connection
							// failure.
							myLog.l(Log.DEBUG, "Incrementing proxy start failures");
							consecutiveProxyStartFailures++;
						} else {
							// Otherwise assume the proxy started successfully and
							// crashed later.
							myLog.l(Log.DEBUG, "Resetting proxy start failures");
							consecutiveProxyStartFailures = 0;
						}
					}
				}
				if(proxyConnector == null) {
					long nowMillis = new Date().getTime();
					boolean shouldStartListener = false;
					// We want to restart the proxy listener without much delay 
					// for the first few attempts, but add a much longer delay 
					// if we consistently fail to connect.
					if(consecutiveProxyStartFailures < 3 
							&& (nowMillis - proxyStartMillis) > 5000) 
					{
						// Retry every 5 seconds for the first 3 tries
						shouldStartListener = true;
					} else if(nowMillis - proxyStartMillis > 30000) {
						// After the first 3 tries, only retry once per 30 sec
						shouldStartListener = true;
					}
					if(shouldStartListener) {
						myLog.l(Log.DEBUG, "Spawning ProxyConnector");
						proxyConnector = new ProxyConnector(this);
						proxyConnector.start();
						proxyStartMillis = nowMillis;
					}
				}
			}
			try {
				// todo: think about using ServerSocket, and just closing
				// the main socket to send an exit signal
				Thread.sleep(WAKE_INTERVAL_MS);
			} catch(InterruptedException e) {
				myLog.l(Log.DEBUG, "Thread interrupted");
			}
		}
			
		terminateAllSessions();

		if(proxyConnector != null) {
			proxyConnector.quit();
			proxyConnector = null;
		}
		if(wifiListener != null) {
			wifiListener.quit();
			wifiListener = null;
		}
		shouldExit = false; // we handled the exit flag, so reset it to acknowledge
		myLog.l(Log.DEBUG, "Exiting cleanly, returning from run()");
	}
	
	private void terminateAllSessions() {
		myLog.i("Terminating " + sessionThreads.size() + " session thread(s)");
		synchronized(this) {
			for(SessionThread sessionThread : sessionThreads) {
				if(sessionThread != null) {
					sessionThread.closeDataSocket();
					sessionThread.closeSocket();
				}
			}
		}
	}
	
	public void cleanupAndStopService() {
		// Call the Android Service shutdown function
		Context context = getApplicationContext();
		Intent intent = new Intent(context,	FTPServerService.class);
		context.stopService(intent);
		if(wifiLock != null) {
			wifiLock.release();
			wifiLock = null;
		}
	}
	
	public void errorShutdown() {
		myLog.l(Log.ERROR, "Service errorShutdown() called");
		cleanupAndStopService();
	}

	/**
	 * Gets the IP address of the wifi connection.
	 * @return The integer IP address if wifi enabled, or null if not.
	 */
	public static InetAddress getWifiIp() {
		Context myContext = Globals.getContext();
		if(myContext == null) {
			throw new NullPointerException("Global context is null");
		}
		WifiManager wifiMgr = (WifiManager)myContext
		                        .getSystemService(Context.WIFI_SERVICE);
		if(isWifiEnabled()) {
			int ipAsInt = wifiMgr.getConnectionInfo().getIpAddress();
			if(ipAsInt == 0) {
				return null;
			} else {
				return Util.intToInet(ipAsInt);
			}
		} else {
			return null;
		}
	}
	
	public static boolean isWifiEnabled() {
		Context myContext = Globals.getContext();
		if(myContext == null) {
			throw new NullPointerException("Global context is null");
		}
		WifiManager wifiMgr = (WifiManager)myContext
		                        .getSystemService(Context.WIFI_SERVICE);
		if(wifiMgr.getWifiState() == WifiManager.WIFI_STATE_ENABLED) {
			return true;
		} else {
			return false;
		}
	}
	
//  Deprecated in favor of Util.ipToString()
//	public static String getWifiIpAsString() {
//		int addr = getWifiIpAsInt();
//		staticLog.l(Log.DEBUG, "IP as int: " + addr);
//		if(addr != 0) {
//			StringBuffer buf = new StringBuffer();
//			buf.append(addr & 0xff).append('.').
//			append((addr >>>= 8) & 0xff).append('.').
//			append((addr >>>= 8) & 0xff).append('.').
//			append((addr >>>= 8) & 0xff);
//			staticLog.l(Log.DEBUG, "Returning IP string: " + buf.toString());
//			return buf.toString();
//		} else {
//			return null;
//		}
//	}
	
//	public static InetAddress getServerAddress() {
//		return serverAddress;
//	}
	
	public static List<String> getSessionMonitorContents() {
		return new ArrayList<String>(sessionMonitor);
	}
	
	public static List<String> getServerLogContents() {
		return new ArrayList<String>(serverLog);
	}
	
	public static void log(int msgLevel, String s) {
		serverLog.add(s);
		int maxSize = Defaults.getServerLogScrollBack();
		while(serverLog.size() > maxSize) {
			serverLog.remove(0);
		}
		//updateClients();
	}
	
	public static void updateClients() {
		UiUpdater.updateClients();
	}
	
	public static void writeMonitor(boolean incoming, String s) {}
//	public static void writeMonitor(boolean incoming, String s) {
//		if(incoming) {
//			s = "> " + s;
//		} else {
//			s = "< " + s;
//		}
//		sessionMonitor.add(s.trim());
//		int maxSize = Defaults.getSessionMonitorScrollBack();
//		while(sessionMonitor.size() > maxSize) {
//			sessionMonitor.remove(0);
//		}
//		updateClients();
//	}

	public static int getPort() {
		return port;
	}

	public static void setPort(int port) {
		FTPServerService.port = port;
	}

	public void registerSessionThread(SessionThread newSession) {
		// Look for finished session threads and stop tracking them in
		// the sessionThreads list
		
		// Since we're not allowed to modify the list while iterating over
		// it, we construct a list in toBeRemoved of threads to remove
		// later from the sessionThreads list.
		synchronized(this) {
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
			sessionThreads.add(newSession);
		}
		myLog.d("Registered session thread");
	}

	/** Get the ProxyConnector, may return null if proxying is disabled. */
	public ProxyConnector getProxyConnector() {
		return proxyConnector;
	}

	static public SharedPreferences getSettings() {
		return settings;
	}
}
