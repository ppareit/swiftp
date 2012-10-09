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

package be.ppareit.swiftp_free;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.WifiLock;
import android.os.IBinder;
import android.os.PowerManager;
import android.preference.PreferenceManager;
import android.util.Log;
import be.ppareit.swiftp_free.server.ProxyConnector;
import be.ppareit.swiftp_free.server.SessionThread;
import be.ppareit.swiftp_free.server.TcpListener;

public class FTPServerService extends Service implements Runnable {
    private static final String TAG = FTPServerService.class.getSimpleName();

    // Service will broadcast (LocalBroadcast) when server start/stop
    static private final String PACKAGE = FTPServerService.class.getPackage().getName();
    static public final String ACTION_STARTED = PACKAGE + ".FTPServerService.STARTED";
    static public final String ACTION_STOPPED = PACKAGE + ".FTPServerService.STOPPED";
    static public final String ACTION_FAILEDTOSTART = PACKAGE
            + ".FTPServerService.FAILEDTOSTART";

    protected static Thread serverThread = null;
    protected boolean shouldExit = false;

    public static final int BACKLOG = 21;
    public static final int MAX_SESSIONS = 5;
    public static final String WAKE_LOCK_TAG = "SwiFTP";

    // protected ServerSocketChannel wifiSocket;
    protected ServerSocket listenSocket;
    protected static WifiLock wifiLock = null;

    // protected static InetAddress serverAddress = null;

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
    protected static boolean fullWake;

    private TcpListener wifiListener = null;
    private ProxyConnector proxyConnector = null;
    private final List<SessionThread> sessionThreads = new ArrayList<SessionThread>();

    private static SharedPreferences settings = null;

    PowerManager.WakeLock wakeLock;

    public FTPServerService() {
    }

    @Override
    public IBinder onBind(Intent intent) {
        // We don't implement this functionality, so ignore it
        return null;
    }

    @Override
    public void onCreate() {
        Log.d(TAG, "SwiFTP server created");
        // Set the application-wide context global, if not already set
        Context myContext = Globals.getContext();
        if (myContext == null) {
            myContext = getApplicationContext();
            if (myContext != null) {
                Globals.setContext(myContext);
            }
        }
        return;
    }

    @Override
    public void onStart(Intent intent, int startId) {
        super.onStart(intent, startId);

        shouldExit = false;
        int attempts = 10;
        // The previous server thread may still be cleaning up, wait for it
        // to finish.
        while (serverThread != null) {
            Log.w(TAG, "Won't start, server thread exists");
            if (attempts > 0) {
                attempts--;
                Util.sleepIgnoreInterupt(1000);
            } else {
                Log.w(TAG, "Server thread already exists");
                return;
            }
        }
        Log.d(TAG, "Creating server thread");
        serverThread = new Thread(this);
        serverThread.start();
    }

    public static boolean isRunning() {
        // return true if and only if a server Thread is running
        if (serverThread == null) {
            Log.d(TAG, "Server is not running (null serverThread)");
            return false;
        }
        if (!serverThread.isAlive()) {
            Log.d(TAG, "serverThread non-null but !isAlive()");
        } else {
            Log.d(TAG, "Server is alive");
        }
        return true;
    }

    @Override
    public void onDestroy() {
        Log.i(TAG, "onDestroy() Stopping server");
        shouldExit = true;
        if (serverThread == null) {
            Log.w(TAG, "Stopping with null serverThread");
            return;
        } else {
            serverThread.interrupt();
            try {
                serverThread.join(10000); // wait 10 sec for server thread to
                                          // finish
            } catch (InterruptedException e) {
            }
            if (serverThread.isAlive()) {
                Log.w(TAG, "Server thread failed to exit");
                // it may still exit eventually if we just leave the
                // shouldExit flag set
            } else {
                Log.d(TAG, "serverThread join()ed ok");
                serverThread = null;
            }
        }
        try {
            if (listenSocket != null) {
                Log.i(TAG, "Closing listenSocket");
                listenSocket.close();
            }
        } catch (IOException e) {
        }

        if (wifiLock != null) {
            wifiLock.release();
            wifiLock = null;
        }
        Log.d(TAG, "FTPServerService.onDestroy() finished");
    }

    private boolean loadSettings() {
        Log.d(TAG, "Loading settings");
        settings = PreferenceManager.getDefaultSharedPreferences(this);
        port = Integer.valueOf(settings.getString("portNum", "2121"));
        if (port == 0) {
            // If port number from settings is invalid, use the default
            port = Defaults.portNumber;
        }
        Log.d(TAG, "Using port " + port);

        acceptNet = settings.getBoolean("allowNet", Defaults.acceptNet);
        acceptWifi = settings.getBoolean("allowWifi", Defaults.acceptWifi);
        fullWake = settings.getBoolean("stayAwake", Defaults.stayAwake);

        // The username, password, and chrootDir are just checked for sanity
        String username = settings.getString("username", null);
        String password = settings.getString("password", null);
        String chrootDir = settings.getString("chrootDir", Defaults.chrootDir);

        validateBlock: {
            if (username == null || password == null) {
                Log.e(TAG, "Username or password is invalid");
                break validateBlock;
            }
            File chrootDirAsFile = new File(chrootDir);
            if (!chrootDirAsFile.isDirectory()) {
                Log.e(TAG, "Chroot dir is invalid");
                break validateBlock;
            }
            Globals.setChrootDir(chrootDirAsFile);
            Globals.setUsername(username);
            return true;
        }
        // We reach here if the settings were not sane
        return false;
    }

    // This opens a listening socket on all interfaces.
    void setupListener() throws IOException {
        listenSocket = new ServerSocket();
        listenSocket.setReuseAddress(true);
        listenSocket.bind(new InetSocketAddress(port));
    }

    public void run() {
        // The UI will want to check the server status to update its
        // start/stop server button
        int consecutiveProxyStartFailures = 0;
        long proxyStartMillis = 0;

        Log.d(TAG, "Server thread running");

        // set our members according to user preferences
        if (!loadSettings()) {
            // loadSettings returns false if settings are not sane
            cleanupAndStopService();
            sendBroadcast(new Intent(ACTION_FAILEDTOSTART));
            return;
        }

        if (isWifiEnabled() == false) {
            cleanupAndStopService();
            sendBroadcast(new Intent(ACTION_FAILEDTOSTART));
            return;
        }

        // Initialization of wifi
        if (acceptWifi) {
            // If configured to accept connections via wifi, then set up the
            // socket
            try {
                setupListener();
            } catch (IOException e) {
                Log.w(TAG, "Error opening port, check your network connection.");
                // serverAddress = null;
                cleanupAndStopService();
                return;
            }
            takeWifiLock();
        }
        takeWakeLock();

        Log.i(TAG, "SwiFTP server ready");

        // A socket is open now, so the FTP server is started, notify rest of world
        sendBroadcast(new Intent(ACTION_STARTED));

        while (!shouldExit) {
            if (acceptWifi) {
                if (wifiListener != null) {
                    if (!wifiListener.isAlive()) {
                        Log.d(TAG, "Joining crashed wifiListener thread");
                        try {
                            wifiListener.join();
                        } catch (InterruptedException e) {
                        }
                        wifiListener = null;
                    }
                }
                if (wifiListener == null) {
                    // Either our wifi listener hasn't been created yet, or has
                    // crashed,
                    // so spawn it
                    wifiListener = new TcpListener(listenSocket, this);
                    wifiListener.start();
                }
            }
            if (acceptNet) {
                if (proxyConnector != null) {
                    if (!proxyConnector.isAlive()) {
                        Log.d(TAG, "Joining crashed proxy connector");
                        try {
                            proxyConnector.join();
                        } catch (InterruptedException e) {
                        }
                        proxyConnector = null;
                        long nowMillis = new Date().getTime();
                        // myLog.l(Log.DEBUG,
                        // "Now:"+nowMillis+" start:"+proxyStartMillis);
                        if (nowMillis - proxyStartMillis < 3000) {
                            // We assume that if the proxy thread crashed within
                            // 3
                            // seconds of starting, it was a startup or
                            // connection
                            // failure.
                            Log.d(TAG, "Incrementing proxy start failures");
                            consecutiveProxyStartFailures++;
                        } else {
                            // Otherwise assume the proxy started successfully
                            // and
                            // crashed later.
                            Log.d(TAG, "Resetting proxy start failures");
                            consecutiveProxyStartFailures = 0;
                        }
                    }
                }
                if (proxyConnector == null) {
                    long nowMillis = new Date().getTime();
                    boolean shouldStartListener = false;
                    // We want to restart the proxy listener without much delay
                    // for the first few attempts, but add a much longer delay
                    // if we consistently fail to connect.
                    if (consecutiveProxyStartFailures < 3
                            && (nowMillis - proxyStartMillis) > 5000) {
                        // Retry every 5 seconds for the first 3 tries
                        shouldStartListener = true;
                    } else if (nowMillis - proxyStartMillis > 30000) {
                        // After the first 3 tries, only retry once per 30 sec
                        shouldStartListener = true;
                    }
                    if (shouldStartListener) {
                        Log.d(TAG, "Spawning ProxyConnector");
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
            } catch (InterruptedException e) {
                Log.d(TAG, "Thread interrupted");
            }
        }

        terminateAllSessions();

        if (proxyConnector != null) {
            proxyConnector.quit();
            proxyConnector = null;
        }
        if (wifiListener != null) {
            wifiListener.quit();
            wifiListener = null;
        }
        shouldExit = false; // we handled the exit flag, so reset it to
                            // acknowledge
        Log.d(TAG, "Exiting cleanly, returning from run()");

        cleanupAndStopService();
    }

    private void terminateAllSessions() {
        Log.i(TAG, "Terminating " + sessionThreads.size() + " session thread(s)");
        synchronized (this) {
            for (SessionThread sessionThread : sessionThreads) {
                if (sessionThread != null) {
                    sessionThread.closeDataSocket();
                    sessionThread.closeSocket();
                }
            }
        }
    }

    public void cleanupAndStopService() {
        // Call the Android Service shutdown function
        stopSelf();
        releaseWifiLock();
        releaseWakeLock();
        sendBroadcast(new Intent(ACTION_STOPPED));
    }

    private void takeWakeLock() {
        if (wakeLock == null) {
            PowerManager pm = (PowerManager) getSystemService(Context.POWER_SERVICE);
            // Many (all?) devices seem to not properly honor a
            // PARTIAL_WAKE_LOCK,
            // which should prevent CPU throttling. This has been
            // well-complained-about on android-developers.
            // For these devices, we have a config option to force the phone
            // into a
            // full wake lock.
            if (fullWake) {
                wakeLock = pm.newWakeLock(PowerManager.FULL_WAKE_LOCK, WAKE_LOCK_TAG);
            } else {
                wakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, WAKE_LOCK_TAG);
            }
            wakeLock.setReferenceCounted(false);
        }
        Log.d(TAG, "Acquiring wake lock");
        wakeLock.acquire();
    }

    private void releaseWakeLock() {
        Log.d(TAG, "Releasing wake lock");
        if (wakeLock != null) {
            wakeLock.release();
            wakeLock = null;
            Log.d(TAG, "Finished releasing wake lock");
        } else {
            Log.i(TAG, "Couldn't release null wake lock");
        }
    }

    private void takeWifiLock() {
        Log.d(TAG, "Taking wifi lock");
        if (wifiLock == null) {
            WifiManager manager = (WifiManager) getSystemService(Context.WIFI_SERVICE);
            wifiLock = manager.createWifiLock("SwiFTP");
            wifiLock.setReferenceCounted(false);
        }
        wifiLock.acquire();
    }

    private void releaseWifiLock() {
        Log.d(TAG, "Releasing wifi lock");
        if (wifiLock != null) {
            wifiLock.release();
            wifiLock = null;
        }
    }

    public void errorShutdown() {
        Log.e(TAG, "Service errorShutdown() called");
        cleanupAndStopService();
    }

    /**
     * Gets the IP address of the wifi connection.
     * 
     * @return The integer IP address if wifi enabled, or null if not.
     */
    public static InetAddress getWifiIp() {
        Context myContext = Globals.getContext();
        if (myContext == null) {
            throw new NullPointerException("Global context is null");
        }
        WifiManager wifiMgr = (WifiManager) myContext
                .getSystemService(Context.WIFI_SERVICE);
        if (isWifiEnabled()) {
            int ipAsInt = wifiMgr.getConnectionInfo().getIpAddress();
            if (ipAsInt == 0) {
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
        if (myContext == null) {
            throw new NullPointerException("Global context is null");
        }
        WifiManager wifiMgr = (WifiManager) myContext
                .getSystemService(Context.WIFI_SERVICE);
        if (wifiMgr.getWifiState() == WifiManager.WIFI_STATE_ENABLED) {
            return true;
        } else {
            return false;
        }
    }

    public static List<String> getSessionMonitorContents() {
        return new ArrayList<String>(sessionMonitor);
    }

    public static List<String> getServerLogContents() {
        return new ArrayList<String>(serverLog);
    }

    public static void log(int msgLevel, String s) {
        serverLog.add(s);
        int maxSize = Defaults.getServerLogScrollBack();
        while (serverLog.size() > maxSize) {
            serverLog.remove(0);
        }
        // updateClients();
    }

    public static void writeMonitor(boolean incoming, String s) {
    }

    // public static void writeMonitor(boolean incoming, String s) {
    // if(incoming) {
    // s = "> " + s;
    // } else {
    // s = "< " + s;
    // }
    // sessionMonitor.add(s.trim());
    // int maxSize = Defaults.getSessionMonitorScrollBack();
    // while(sessionMonitor.size() > maxSize) {
    // sessionMonitor.remove(0);
    // }
    // updateClients();
    // }

    public static int getPort() {
        return port;
    }

    public static void setPort(int port) {
        FTPServerService.port = port;
    }

    /**
     * The FTPServerService must know about all running session threads so they can be
     * terminated on exit. Called when a new session is created.
     */
    public void registerSessionThread(SessionThread newSession) {
        // Before adding the new session thread, clean up any finished session
        // threads that are present in the list.

        // Since we're not allowed to modify the list while iterating over
        // it, we construct a list in toBeRemoved of threads to remove
        // later from the sessionThreads list.
        synchronized (this) {
            List<SessionThread> toBeRemoved = new ArrayList<SessionThread>();
            for (SessionThread sessionThread : sessionThreads) {
                if (!sessionThread.isAlive()) {
                    Log.d(TAG, "Cleaning up finished session...");
                    try {
                        sessionThread.join();
                        Log.d(TAG, "Thread joined");
                        toBeRemoved.add(sessionThread);
                        sessionThread.closeSocket(); // make sure socket closed
                    } catch (InterruptedException e) {
                        Log.d(TAG, "Interrupted while joining");
                        // We will try again in the next loop iteration
                    }
                }
            }
            for (SessionThread removeThread : toBeRemoved) {
                sessionThreads.remove(removeThread);
            }

            // Cleanup is complete. Now actually add the new thread to the list.
            sessionThreads.add(newSession);
        }
        Log.d(TAG, "Registered session thread");
    }

    /** Get the ProxyConnector, may return null if proxying is disabled. */
    public ProxyConnector getProxyConnector() {
        return proxyConnector;
    }

    static public SharedPreferences getSettings() {
        return settings;
    }
}
