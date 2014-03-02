/*
Copyright 2011-2013 Pieter Pareit

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

import android.annotation.TargetApi;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.nsd.NsdManager;
import android.net.nsd.NsdManager.RegistrationListener;
import android.net.nsd.NsdServiceInfo;
import android.os.Build;
import android.os.IBinder;
import android.util.Log;

@TargetApi(Build.VERSION_CODES.JELLY_BEAN)
public class NsdService extends Service {
    private static final String TAG = NsdService.class.getSimpleName();

    private String mServiceName = "Android FTP Server";
    private NsdManager mNsdManager = null;

    public static class StartStopReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.d(TAG, "onReceive broadcast: " + intent.getAction());

            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN) {
                Log.w(TAG, "onReceive: Running pre-JB, version to old for");
                Log.w(TAG, "onReceive: NSD functionallity, bailing out");
                return;
            }
            if (intent.getAction().equals(FtpServerService.ACTION_STARTED)) {
                Intent service = new Intent(context, NsdService.class);
                context.startService(service);
            } else if (intent.getAction().equals(FtpServerService.ACTION_STOPPED)) {
                Intent service = new Intent(context, NsdService.class);
                context.stopService(service);
            }
        }

    }

    private RegistrationListener mRegistrationListener = new RegistrationListener() {

        @Override
        public void onServiceRegistered(NsdServiceInfo serviceInfo) {
            Log.d(TAG, "onServiceRegistered: " + serviceInfo.getServiceName());
            mServiceName = serviceInfo.getServiceName();
        }

        @Override
        public void onServiceUnregistered(NsdServiceInfo serviceInfo) {
            Log.d(TAG, "onServiceUnregistered: " + serviceInfo.getServiceName());
        }

        @Override
        public void onRegistrationFailed(NsdServiceInfo serviceInfo, int errorCode) {
            Log.d(TAG, "onRegistrationFailed: errorCode=" + errorCode);
        }

        @Override
        public void onUnregistrationFailed(NsdServiceInfo serviceInfo, int errorCode) {
            Log.d(TAG, "onUnregistrationFailed: errorCode=" + errorCode);
        }
    };

    @Override
    public void onCreate() {
        NsdServiceInfo serviceInfo = new NsdServiceInfo();
        serviceInfo.setServiceName(mServiceName);
        serviceInfo.setServiceType("_ftp._tcp");
        serviceInfo.setPort(Settings.getPortNumber());

        mNsdManager = (NsdManager) getSystemService(Context.NSD_SERVICE);
        mNsdManager.registerService(serviceInfo, NsdManager.PROTOCOL_DNS_SD,
                mRegistrationListener);
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        if (mNsdManager == null) {
            Log.e(TAG, "unregisterService: Unexpected mNsdManger to be null, bailing out");
            return;
        }
        mNsdManager = (NsdManager) getSystemService(Context.NSD_SERVICE);
        mNsdManager.unregisterService(mRegistrationListener);
        mNsdManager = null;
    }
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

}
