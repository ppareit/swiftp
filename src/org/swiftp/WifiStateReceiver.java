/*package org.swiftp;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.NetworkInfo;
import android.net.NetworkInfo.State;
import android.net.wifi.WifiManager;
import android.util.Log;

public class WifiStateReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent wifiIntent) {
		MyLog myLog = new MyLog(this.getClass().getName());
		myLog.l(Log.DEBUG, "WifiStateReceiver running!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
		SharedPreferences settings;
		if(context == null) {
			myLog.l(Log.INFO, "Null context in WifiStateReceiver");
			return;
		}
		settings = context.getSharedPreferences(Defaults.getSettingsName(),
				Defaults.getSettingsMode());
		if(wifiIntent.getAction() == WifiManager.NETWORK_STATE_CHANGED_ACTION) {
			NetworkInfo netInfo;
			netInfo = (NetworkInfo)wifiIntent.getParcelableExtra(
					WifiManager.EXTRA_NETWORK_INFO);
			Intent intent = new Intent(context,	FTPServerService.class);
			if(netInfo.getState() == State.CONNECTED) {
				myLog.l(Log.DEBUG, "Wifi up");
				if(settings.getBoolean("autostart", false) == true) {
					myLog.l(Log.INFO, "Autostart on, starting server");
						context.startService(intent);
				}
			} else {
				// Wifi is connecting or shutting down, but not up
				myLog.l(Log.INFO, "Stopping service due to unconnected wifi");
				context.stopService(intent);
			}
		}
	}

}
*/