package be.ppareit.swiftp;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

public class RequestStartStopReceiver extends BroadcastReceiver {

    static final String TAG = RequestStartStopReceiver.class.getSimpleName();

    @Override
    public void onReceive(Context context, Intent intent) {
        Log.v(TAG, "Received: " + intent.getAction());

        // TODO: analog code as in ServerPreferenceActivity.start/stopServer(), refactor
        if (intent.getAction().equals(FtpServerService.ACTION_START_FTPSERVER)) {
            Intent serverService = new Intent(context, FtpServerService.class);
            if (!FtpServerService.isRunning()) {
                // warnIfNoExternalStorage();
                context.startService(serverService);
            }
        } else if (intent.getAction().equals(FtpServerService.ACTION_STOP_FTPSERVER)) {
            Intent serverService = new Intent(context, FtpServerService.class);
            context.stopService(serverService);
        }
    }

}
