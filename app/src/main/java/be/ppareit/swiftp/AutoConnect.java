/*
Copyright 2011-2015 Pieter Pareit

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

import android.app.IntentService;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.widget.Toast;

import net.vrallev.android.cat.Cat;

/**
 * The class AutoConnect is responsible to start or stop the server depending on network
 * changes.
 */
public class AutoConnect {

    static public class WifiStateChangedReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (!WifiManager.NETWORK_STATE_CHANGED_ACTION.equals(intent.getAction())) {
                Cat.w("Received broadcast receiver for wrong action.");
                return;
            }
            NetworkInfo info = intent.getParcelableExtra(WifiManager.EXTRA_NETWORK_INFO);
            if (info == null) {
                Cat.e("Null network info received, bailing");
                return;
            }
            if (info.isConnected()) {
                Cat.d("We are connecting to a wifi network");
                Intent startServerIntent = new Intent(context, StartServerService.class);
                context.startService(startServerIntent);
            } else if (info.isConnectedOrConnecting()) {
                Cat.v("Still connecting, ignoring");
            } else {
                Cat.d("We are disconnected from wifi network");
                Intent stopServerIntent = new Intent(context, StopServerService.class);
                context.startService(stopServerIntent);
            }
        }
    }

    /**
     * When the ftp stops, for instance when the user stops the server
     * and when we are still connected to an auto connection network
     * we should stop from auto connecting to this network.
     */
    static public class ServerStoppedReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Cat.d("Server stopped received");
            if (FsSettings.getAutoConnectList().contains(getConnectedWifiSSID(context))) {
                Toast.makeText(context, "Stopping FTP Server while connected to a wifi network we automatically connected to. Removing this wifi network from the preferences", Toast.LENGTH_LONG).show();
                FsSettings.removeFromAutoConnectList(getConnectedWifiSSID(context));
            }
        }
    }

    static public class StartServerService extends IntentService {

        public StartServerService() {
            super(StartServerService.class.getSimpleName());
        }

        @Override
        protected void onHandleIntent(Intent intent) {
            if (FsService.isRunning()) {
                Cat.v("We are connecting to a new wifi network on a running server, ignore");
                return;
            }
            if (FsSettings.getAutoConnectList().contains(getConnectedWifiSSID(this))) {
                // sleep a short while so the network has time to truly connect
                Util.sleepIgnoreInterrupt(1000);
                sendBroadcast(new Intent(FsService.ACTION_START_FTPSERVER));
            }
        }
    }

    static public class StopServerService extends IntentService {

        public StopServerService() {
            super(StopServerService.class.getSimpleName());
        }

        @Override
        protected void onHandleIntent(Intent intent) {
            if (!FsService.isRunning()) return;

            Util.sleepIgnoreInterrupt(15000);
            if (!FsService.isRunning()) return;

            ConnectivityManager conManager = (ConnectivityManager) getSystemService(CONNECTIVITY_SERVICE);
            NetworkInfo netInfo = conManager.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
            if (netInfo.isConnectedOrConnecting()) return;

            Cat.d("Wifi connection disconnected and no longer connecting, stopping the ftp server");
            sendBroadcast(new Intent(FsService.ACTION_STOP_FTPSERVER));
        }
    }

    private static String getConnectedWifiSSID(Context context) {
        Context appContext = context.getApplicationContext();
        WifiManager wifiManager = (WifiManager) appContext.getSystemService(Context.WIFI_SERVICE);
        if (wifiManager == null) {
            Cat.e("Unable to get the WifiManager");
            return "";
        }
        WifiInfo wifiInfo = wifiManager.getConnectionInfo();
        if (wifiInfo == null) {
            Cat.e("Null wifi info received, bailing");
            return "";
        }
        Cat.d("We are connected to " + wifiInfo.getSSID());
        return wifiInfo.getSSID();
    }
}


















