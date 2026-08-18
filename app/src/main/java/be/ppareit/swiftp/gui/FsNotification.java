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

package be.ppareit.swiftp.gui;

import static android.content.Context.NOTIFICATION_SERVICE;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;

import net.vrallev.android.cat.Cat;

import java.net.InetAddress;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;

public class FsNotification {

    public static final int NOTIFICATION_ID = 7890;

    private static final String CHANNEL_ID = "be.ppareit.swiftp.notification.channelId";

    /**
     * Will the user really see the notification?
     */
    public static boolean isVisible(Context context) {
        if (!NotificationManagerCompat.from(context).areNotificationsEnabled()) return false;
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return true;
        NotificationManager nm = (NotificationManager) context.getSystemService(NOTIFICATION_SERVICE);
        if (nm == null) return false;
        NotificationChannel channel = nm.getNotificationChannel(CHANNEL_ID);
        // no channel yet means it was never created, so nothing blocked it either
        return channel == null || channel.getImportance() != NotificationManager.IMPORTANCE_NONE;
    }

    /**
     * The server address as shown to the user, "-" as long as there is no address.
     */
    public static String getServerAddressText() {
        InetAddress address = FsService.getLocalInetAddress();
        String ipText;
        if (address == null) ipText = "-";
        else ipText = "ftp://" + address.getHostAddress() + ":" + FsSettings.getPortNumber();
        if (FsSettings.isImplicitUsed()) ipText += ", " + FsSettings.getPortNumberImplicit();
        return ipText + "/";
    }

    public static Notification setupNotification(Context context) {
        Cat.d("Setting up the notification");
        // Get NotificationManager reference
        NotificationManager nm = (NotificationManager) context.getSystemService(NOTIFICATION_SERVICE);

        String ipText = getServerAddressText();

        // Instantiate a Notification
        int icon = R.mipmap.notification;
        CharSequence tickerText = String.format(context.getString(R.string.notification_server_starting), ipText);
        long when = System.currentTimeMillis();

        // Define Notification's message and Intent
        CharSequence contentTitle = context.getString(R.string.notification_title);
        CharSequence contentText = String.format(context.getString(R.string.notification_text), ipText);

        Intent notificationIntent = new Intent(context, MainActivity.class);
        notificationIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent contentIntent = PendingIntent.getActivity(context, 0,
                notificationIntent, PendingIntent.FLAG_IMMUTABLE);

        int stopIcon = android.R.drawable.ic_menu_close_clear_cancel;
        CharSequence stopText = context.getString(R.string.notification_stop_text);
        Intent stopIntent = new Intent(context, FsService.class);
        stopIntent.setAction(FsService.ACTION_REQUEST_STOP);
        PendingIntent stopPendingIntent = PendingIntent.getService(context, 0,
                stopIntent, PendingIntent.FLAG_ONE_SHOT | PendingIntent.FLAG_IMMUTABLE);

        int preferenceIcon = android.R.drawable.ic_menu_preferences;
        CharSequence preferenceText = context.getString(R.string.notif_settings_text);
        Intent preferenceIntent = new Intent(context, MainActivity.class);
        preferenceIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
        PendingIntent preferencePendingIntent = PendingIntent.getActivity(context, 0,
                preferenceIntent, PendingIntent.FLAG_IMMUTABLE);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Show FTP Server state";
            String description = "This notification shows the current state of the FTP Server";
            int importance = NotificationManager.IMPORTANCE_DEFAULT;
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID, name, importance);
            channel.setDescription(description);
            nm.createNotificationChannel(channel);
        }

        Notification notification = new NotificationCompat.Builder(context)
                .setContentTitle(contentTitle)
                .setContentText(contentText)
                .setContentIntent(contentIntent)
                .setSmallIcon(icon)
                .setTicker(tickerText)
                .setWhen(when)
                .setOngoing(true)
                .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                .setCategory(NotificationCompat.CATEGORY_SERVICE)
                .setPriority(Notification.PRIORITY_DEFAULT)  // Ignored on API >= 26
                .addAction(stopIcon, stopText, stopPendingIntent)
                .addAction(preferenceIcon, preferenceText, preferencePendingIntent)
                .setShowWhen(false)
                .setChannelId(CHANNEL_ID)
                .build();

        // Pass Notification to NotificationManager
        return notification;
    }

}
