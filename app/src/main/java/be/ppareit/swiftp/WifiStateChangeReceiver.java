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

import net.vrallev.android.cat.Cat;

/**
 * Detect if we get on a wifi network and possible start server.
 */
public class WifiStateChangeReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        NetworkInfo info = intent.getParcelableExtra(WifiManager.EXTRA_NETWORK_INFO);
        if (info == null) {
            Cat.e("Null network info received, bailing");
            return;
        }
        if (info.isConnected()) {
            Cat.d("We are connecting to a wifi network");
            Intent startServerIntent = new Intent(context, StartServerService.class);
            context.startService(startServerIntent);
        } else {
            Cat.d("We are disconnected from wifi network");
            Intent stopServerIntent = new Intent(context, StopServerService.class);
            context.startService(stopServerIntent);
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
            WifiManager wifiManager = (WifiManager) getSystemService(Context.WIFI_SERVICE);
            WifiInfo wifiInfo = wifiManager.getConnectionInfo();
            if (wifiInfo == null) {
                Cat.e("Null wifi info received, bailing");
                return;
            }
            Cat.d("We are connected to " + wifiInfo.getSSID());
            if (FsSettings.getAutoConnectList().contains(wifiInfo.getSSID())) {
                // sleep a short while so the network has time to truly connect
                Util.sleepIgnoreInterupt(1000);
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

            Util.sleepIgnoreInterupt(15000);
            if (!FsService.isRunning()) return;

            ConnectivityManager conManager = (ConnectivityManager) getSystemService(CONNECTIVITY_SERVICE);
            NetworkInfo netInfo = conManager.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
            if (netInfo.isConnectedOrConnecting()) return;

            Cat.d("Wifi connection disconnected and no longer connecting, stopping the ftp server");
            sendBroadcast(new Intent(FsService.ACTION_STOP_FTPSERVER));
        }
    }
}



















