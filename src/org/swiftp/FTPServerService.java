/**
 * 
 */
package org.swiftp;

import android.app.Service;
import android.content.Intent;
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
	protected MyLog myLog = new MyLog(this.getClass().getName());
	
	public FTPServerService() {
		// Don't need to do any initialization
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
		UiUpdater.updateClients();
		// todo: we should broadcast an intent to inform anyone who cares
	}
	
	public void run() {
		do {
			UiUpdater.updateClients();
			try {
				myLog.l(Log.DEBUG, "Thread is running");
				Thread.sleep(20000);
			} catch (InterruptedException e) {
				myLog.l(Log.DEBUG, "Thread interrupted");
			}
		} while (!shouldExit);
		myLog.l(Log.DEBUG, "Exiting cleanly");
		shouldExit = false; // we handled the exit flag, so reset it
	}
}
