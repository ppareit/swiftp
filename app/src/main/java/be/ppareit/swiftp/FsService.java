/*
Copyright 2011-2013 Pieter Pareit
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

package be.ppareit.swiftp;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.content.SharedPreferences;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.WifiLock;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Message;
import android.os.PowerManager;
import android.os.SystemClock;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.Gravity;
import android.widget.Toast;

import androidx.core.content.ContextCompat;

import net.vrallev.android.cat.Cat;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.NetworkInterface;
import java.net.ServerSocket;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import be.ppareit.swiftp.gui.FsNotification;
import be.ppareit.swiftp.server.SessionThread;
import be.ppareit.swiftp.server.TcpListener;

public class FsService extends Service implements Runnable {
    private static final String TAG = FsService.class.getSimpleName();

    // Service will check following actions when started through intent
    static public final String ACTION_REQUEST_START = "be.ppareit.swiftp.REQUEST_START";
    static public final String ACTION_REQUEST_STOP = "be.ppareit.swiftp.REQUEST_STOP";

    // Service will (global) broadcast when server start/stop
    static public final String ACTION_STARTED = "be.ppareit.swiftp.FTPSERVER_STARTED";
    static public final String ACTION_STOPPED = "be.ppareit.swiftp.FTPSERVER_STOPPED";
    static public final String ACTION_FAILEDTOSTART = "be.ppareit.swiftp.FTPSERVER_FAILEDTOSTART";

    protected static Thread serverThread = null;
    protected boolean shouldExit = false;

    protected ServerSocket listenSocket;

    private TcpListener socketWatcher = null;
    private final List<SessionThread> sessionThreads = new ArrayList<>();

    private PowerManager.WakeLock wakeLock;
    private WifiLock wifiLock = null;

    private static boolean connectionWakelockRunning = false;
    static Handler connWakeLockHandler = null;
    static Message connWakeLockMessage = null;
    static boolean useConnWakeLocks = false;


    /**
     * Check to see if the FTP Server is up and running
     *
     * @return true if the FTP Server is up and running
     */
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

    /**
     * Start this service, which will start the FTP Server
     */
    public static void start() {
        Context context = App.getAppContext();
        Intent serviceIntent = new Intent(context, FsService.class);
        if (!FsService.isRunning()) {
            ContextCompat.startForegroundService(context, serviceIntent);
        }
    }

    /**
     * Stop the service and thus stop the FTP Server
     */
    public static void stop() {
        Context context = App.getAppContext();
        Intent serverService = new Intent(context, FsService.class);
        context.stopService(serverService);
    }

    /**
     * Will check if the device contains external storage (sdcard) and display a warning
     * for the user if there is no external storage. Nothing more.
     */
    private static void warnIfNoExternalStorage() {
        String storageState = Environment.getExternalStorageState();
        if (!storageState.equals(Environment.MEDIA_MOUNTED)) {
            Log.v(TAG, "Warning due to storage state " + storageState);
            Toast toast = Toast.makeText(App.getAppContext(),
                    R.string.storage_warning, Toast.LENGTH_LONG);
            toast.setGravity(Gravity.CENTER, 0, 0);
            toast.show();
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        if (Build.VERSION.SDK_INT >= 34) {
            startForeground(FsNotification.NOTIFICATION_ID, FsNotification.setupNotification(getApplicationContext()), ServiceInfo.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE);
        } else {
            startForeground(FsNotification.NOTIFICATION_ID, FsNotification.setupNotification(getApplicationContext()));
        }

        //https://developer.android.com/reference/android/app/Service.html
        //if there are not any pending start commands to be delivered to the service, it will be called with a null intent object,
        if (intent != null && intent.getAction() != null) {
            Cat.d("onStartCommand called with action: " + intent.getAction());

            switch (intent.getAction()) {
                case ACTION_REQUEST_START:
                    if (isRunning()) {
                        return START_STICKY;
                    }
                    break;
                case ACTION_REQUEST_STOP:
                    stopSelf();
                    return START_NOT_STICKY;
            }
        }

        warnIfNoExternalStorage();

        shouldExit = false;
        if (serverThread != null) {
            Log.e(TAG, "Server thread already exists: just start");
            return START_STICKY;
        }
        Log.d(TAG, "Creating server thread");
        serverThread = new Thread(this);
        serverThread.start();
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        Log.i(TAG, "onDestroy() Stopping server");
        shouldExit = true;

        endServer();

        if (serverThread == null) {
            Log.w(TAG, "Stopping with null serverThread");
            return;
        }
        serverThread.interrupt();
        try {
            serverThread.join(10000); // wait 10 sec for server thread to finish
        } catch (InterruptedException ignored) {
        }
        if (serverThread.isAlive()) {
            Log.w(TAG, "Server thread failed to exit");
            // it may still exit eventually if we just leave the shouldExit flag set
        } else {
            Log.d(TAG, "serverThread join()ed ok");
            serverThread = null;
        }
        try {
            if (listenSocket != null) {
                Log.i(TAG, "Closing listenSocket");
                listenSocket.close();
            }
        } catch (IOException ignored) {
        }

        releaseWakelocks();

        if (connWakeLockHandler != null) connWakeLockHandler.removeCallbacksAndMessages(null);
        if (connWakeLockMessage != null) connWakeLockMessage = null;

        Log.d(TAG, "FTPServerService.onDestroy() finished");
    }

    private void endServer() {
        terminateAllSessions();

        if (socketWatcher != null) {
            socketWatcher.quit();
            socketWatcher = null;
        }
        shouldExit = false; // we handled the exit flag, so reset it to acknowledge
        Log.d(TAG, "Exiting cleanly, returning from run()");

        stopSelf();
        sendBroadcast(new Intent(ACTION_STOPPED));
    }

    // This opens a listening socket on all interfaces.
    void setupListener() throws IOException {
        listenSocket = new ServerSocket();
        listenSocket.setReuseAddress(true);
        listenSocket.bind(new InetSocketAddress(FsSettings.getPortNumber()));
    }

    @Override
    public void run() {
        Log.d(TAG, "Server thread running");

        if (!isConnectedToLocalNetwork()) {
            Log.w(TAG, "run: There is no local network, bailing out");
            stopSelf();
            sendBroadcast(new Intent(ACTION_FAILEDTOSTART));
            return;
        }

        // Initialization of wifi, set up the socket
        try {
            setupListener();
        } catch (IOException e) {
            Log.w(TAG, "run: Unable to open port, bailing out.");
            stopSelf();
            sendBroadcast(new Intent(ACTION_FAILEDTOSTART));
            return;
        }

        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
        final int batterySaver = Integer.parseInt(sp.getString("battery_saver", "1"));
        if (batterySaver == 0) {
            // @TODO: when using ethernet, is it needed to take wifi lock?
            takeWifiLock();
            takeWakeLock();
        } else if (batterySaver == 1) {
            useConnWakeLocks = true;
            initializeConnWakeLocks();
        }

        // A socket is open now, so the FTP server is started, notify rest of world
        Log.i(TAG, "Ftp Server up and running, broadcasting ACTION_STARTED");
        sendBroadcast(new Intent(ACTION_STARTED));

        socketWatcher = new TcpListener(listenSocket, this);
        socketWatcher.start();
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

    /**
     * Takes the wake lock
     * <p>
     * Many devices seem to not properly honor a PARTIAL_WAKE_LOCK, which should prevent
     * CPU throttling. For these devices, we have a option to force the phone into a full
     * wake lock.
     */
    public void takeWakeLock() {
        if (wakeLock == null) {
            PowerManager pm = (PowerManager) getSystemService(Context.POWER_SERVICE);
            if (FsSettings.shouldTakeFullWakeLock()) {
                Log.d(TAG, "takeWakeLock: Taking full wake lock");
                // Note: FULL_WAKE_LOCK is deprecated, officially not recommended, and is actually worse.
                wakeLock = pm.newWakeLock(PowerManager.FULL_WAKE_LOCK, TAG);
            } else {
                Log.d(TAG, "maybeTakeWakeLock: Taking partial wake lock");
                wakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, TAG);
            }
            wakeLock.setReferenceCounted(false);
        }
        wakeLock.acquire();
    }

    public void takeWifiLock() {
        Log.d(TAG, "takeWifiLock: Taking wifi lock");
        if (wifiLock == null) {
            WifiManager manager = (WifiManager) getApplicationContext().getSystemService(Context.WIFI_SERVICE);
            if (Build.VERSION.SDK_INT >= 29) {
                // Low is forced starting in Android 14 and starts use at Android 10.
                wifiLock = manager.createWifiLock(WifiManager.WIFI_MODE_FULL_LOW_LATENCY, TAG);
            } else {
                wifiLock = manager.createWifiLock(TAG);
            }
            wifiLock.setReferenceCounted(false);
        }
        wifiLock.acquire();
    }

    public void releaseWakelocks() {
        if (wifiLock != null) {
            Log.d(TAG, "onDestroy: Releasing wifi lock");
            wifiLock.release();
            wifiLock = null;
        }
        if (wakeLock != null) {
            Log.d(TAG, "onDestroy: Releasing wake lock");
            wakeLock.release();
            wakeLock = null;
        }
        setConnWakelockNotRunning();
    }

    public boolean isConnWakelockRunning() {
        return connectionWakelockRunning;
    }

    public void setConnWakelockNotRunning() {
        //logging.appendLog("connection wakelocks (off)...");
        connectionWakelockRunning = false;
    }

    public void setConnWakelockRunning() {
        //logging.appendLog("connection wakelocks (on)...");
        connectionWakelockRunning = true;
    }

    public void initializeConnWakeLocks() {
        if (connWakeLockHandler != null) return;
        connWakeLockHandler = new Handler(Looper.getMainLooper()) {
            @Override
            public void handleMessage(@androidx.annotation.NonNull Message msg) {
                super.handleMessage(msg);
                if (isConnWakelockRunning()) releaseWakelocks();
            }
        };
    }

    public void createConnWakeLock() {
        if (!useConnWakeLocks) return;
        if (connWakeLockMessage != null) {
            connWakeLockHandler.removeCallbacksAndMessages(null);
            connWakeLockMessage = null;
        }
        if (!isConnWakelockRunning()) {
            takeWakeLock();
            takeWifiLock();
            setConnWakelockRunning();
        }
    }

    /* Handle a delayed reaction to ending connection wakelocks.
     * As its not possible to know when it will end since ftp client can quit and connect right back
     * again, need to create a delayed reaction. A handler with a delayed message works well.
     * */
    public static void connWakelockEndHandler() {
        if (!useConnWakeLocks) return;
        if (connWakeLockMessage == null) {
            //new Logging().appendLog("connection wakelocks to off in 10 minutes...\n\n\n");
            connWakeLockMessage = connWakeLockHandler.obtainMessage();
            connWakeLockHandler.sendMessageDelayed(connWakeLockMessage, 600000);
        }
    }

    /**
     * Gets the local ip address
     *
     * @return local ip address or null if not found
     */
    public static InetAddress getLocalInetAddress() {
        InetAddress returnAddress = null;
        if (!isConnectedToLocalNetwork()) {
            Log.e(TAG, "getLocalInetAddress called and no connection");
            return null;
        }
        try {
            ArrayList<NetworkInterface> networkInterfaces = Collections.list(NetworkInterface.getNetworkInterfaces());
            for (NetworkInterface networkInterface : networkInterfaces) {
                // only check network interfaces that give local connection
                if (!networkInterface.getName().matches("^(eth|wlan|tun).*"))
                    continue;
                for (InetAddress address : Collections.list(networkInterface.getInetAddresses())) {
                    if (!address.isLoopbackAddress()
                            && !address.isLinkLocalAddress()
                            && address.isSiteLocalAddress()
                            && address instanceof Inet4Address) {
                        if (returnAddress != null) {
                            Cat.w("Found more than one valid address local inet address, why???");
                        }
                        returnAddress = address;
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return returnAddress;
    }

    /**
     * Checks to see if we are connected to a local network, for instance wifi or ethernet
     *
     * @return true if connected to a local network
     */
    public static boolean isConnectedToLocalNetwork() {
        boolean connected = false;
        Context context = App.getAppContext();
        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo ni = cm.getActiveNetworkInfo();
        connected = ni != null && ni.isConnected();
        if (!connected) {
            Log.d(TAG, "isConnectedToLocalNetwork: see if it is an WIFI AP");
            WifiManager wm = (WifiManager) context.getApplicationContext().getSystemService(Context.WIFI_SERVICE);
            try {
                Method method = wm.getClass().getDeclaredMethod("isWifiApEnabled");
                connected = (Boolean) method.invoke(wm);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        if (!connected) {
            Log.d(TAG, "isConnectedToLocalNetwork: see if it is an USB AP");
            try {
                ArrayList<NetworkInterface> networkInterfaces = Collections.list(NetworkInterface.getNetworkInterfaces());
                for (NetworkInterface netInterface : networkInterfaces) {
                    if (netInterface.getDisplayName().startsWith("rndis")) {
                        connected = true;
                    }
                }
            } catch (SocketException e) {
                e.printStackTrace();
            }
        }
        return connected;
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

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onTaskRemoved(Intent rootIntent) {
        super.onTaskRemoved(rootIntent);
        Log.d(TAG, "user has removed my activity, we got killed! restarting...");
        Intent restartService = new Intent(getApplicationContext(), this.getClass());
        restartService.setPackage(getPackageName());
        PendingIntent restartServicePI = PendingIntent.getService(
                getApplicationContext(), 1, restartService, PendingIntent.FLAG_ONE_SHOT);
        AlarmManager alarmService = (AlarmManager) getApplicationContext()
                .getSystemService(Context.ALARM_SERVICE);
        alarmService.set(AlarmManager.ELAPSED_REALTIME,
                SystemClock.elapsedRealtime() + 2000, restartServicePI);
    }

}
