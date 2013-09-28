package be.ppareit.swiftp;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import android.os.*;
import android.widget.*;
import android.view.*;
import be.ppareit.swiftp_free.*;

public class RequestStartStopReceiver extends BroadcastReceiver {

    static final String TAG = RequestStartStopReceiver.class.getSimpleName();

    @Override
    public void onReceive(Context context, Intent intent) {
        Log.v(TAG, "Received: " + intent.getAction());

        // TODO: analog code as in ServerPreferenceActivity.start/stopServer(), refactor
        if (intent.getAction().equals(FtpServerService.ACTION_START_FTPSERVER)) {
            Intent serverService = new Intent(context, FtpServerService.class);
            if (!FtpServerService.isRunning()) {
                warnIfNoExternalStorage();
                context.startService(serverService);
            }
        } else if (intent.getAction().equals(FtpServerService.ACTION_STOP_FTPSERVER)) {
            Intent serverService = new Intent(context, FtpServerService.class);
            context.stopService(serverService);
        }
    }

	/**
     * Will check if the device contains external storage (sdcard) and display a warning
     * for the user if there is no external storage. Nothing more.
     */
    private void warnIfNoExternalStorage() {
        String storageState = Environment.getExternalStorageState();
        if (!storageState.equals(Environment.MEDIA_MOUNTED)) {
            Log.v(TAG, "Warning due to storage state " + storageState);
            Toast toast = Toast.makeText(FtpServerApp.getAppContext(),
				R.string.storage_warning, Toast.LENGTH_LONG);
            toast.setGravity(Gravity.CENTER, 0, 0);
            toast.show();
        }
    }
	
}
