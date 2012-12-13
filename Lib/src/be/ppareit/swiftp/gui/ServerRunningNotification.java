package be.ppareit.swiftp.gui;

import java.net.InetAddress;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import be.ppareit.swiftp.FTPServerService;
import be.ppareit.swiftp.R;

public class ServerRunningNotification extends BroadcastReceiver {
    private static final String TAG = ServerRunningNotification.class.getSimpleName();

    private final int NOTIFICATIONID = 7890;

    @Override
    public void onReceive(Context context, Intent intent) {
        Log.d(TAG, "onReceive broadcast: " + intent.getAction());
        if (intent.getAction().equals(FTPServerService.ACTION_STARTED)) {
            setupNotification(context);
        } else if (intent.getAction().equals(FTPServerService.ACTION_STOPPED)) {
            clearNotification(context);
        }
    }

    private void setupNotification(Context context) {
        Log.d(TAG, "Setting up the notification");
        // Get NotificationManager reference
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager nm = (NotificationManager) context.getSystemService(ns);

        // Instantiate a Notification
        int icon = R.drawable.notification;
        CharSequence tickerText = context.getString(R.string.notif_server_starting);
        long when = System.currentTimeMillis();
        Notification notification = new Notification(icon, tickerText, when);

        // Define Notification's message and Intent
        CharSequence contentTitle = context.getString(R.string.notif_title);
        InetAddress address = FTPServerService.getLocalInetAddress();
        if (address == null) {
            Log.w(TAG, "Unable to retreive the local ip address");
            return;
        }
        String iptext = "ftp://" + address.getHostAddress() + ":"
                + FTPServerService.getPort() + "/";
        CharSequence contentText = String.format(context.getString(R.string.notif_text),
                iptext);
        Intent notificationIntent = new Intent(context, ServerPreferenceActivity.class);
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP
                | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent contentIntent = PendingIntent.getActivity(context, 0,
                notificationIntent, 0);
        notification
                .setLatestEventInfo(context, contentTitle, contentText, contentIntent);
        notification.flags |= Notification.FLAG_ONGOING_EVENT;

        // Pass Notification to NotificationManager
        nm.notify(NOTIFICATIONID, notification);

        Log.d(TAG, "Notication setup done");
    }

    private void clearNotification(Context context) {
        Log.d(TAG, "Clearing the notifications");
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager nm = (NotificationManager) context.getSystemService(ns);
        nm.cancelAll();
        Log.d(TAG, "Cleared notification");
    }
}
