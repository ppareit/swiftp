/*******************************************************************************
 * Copyright (c) 2013 Pieter Pareit.
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * <p>
 * Contributors:
 * Pieter Pareit - initial API and implementation
 ******************************************************************************/
package be.ppareit.swiftp.gui;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.appwidget.AppWidgetManager;
import android.appwidget.AppWidgetProvider;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.IBinder;
import android.widget.RemoteViews;

import androidx.core.app.NotificationCompat;
import androidx.core.content.ContextCompat;

import net.vrallev.android.cat.Cat;

import java.net.InetAddress;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.R;

/**
 * Simple widget for FTP Server.
 *
 * @author ppareit
 */
public class FsWidgetProvider extends AppWidgetProvider {

    @Override
    public void onReceive(Context context, Intent intent) {
        Cat.v("Received broadcast: " + intent.getAction());
        // watch for the broadcasts by the ftp server and update the widget if needed
        final String action = intent.getAction();
        if (FsService.ACTION_STARTED.equals(action) || FsService.ACTION_STOPPED.equals(action)) {
            Intent updateIntent = new Intent(context, UpdateService.class);
            ContextCompat.startForegroundService(context, updateIntent);
        }
        super.onReceive(context, intent);
    }

    @Override
    public void onUpdate(Context context, AppWidgetManager appWidgetManager,
                         int[] appWidgetIds) {
        Cat.d("onUpdated called");
        // let the updating happen by a service
        Intent updateIntent = new Intent(context, UpdateService.class);
        ContextCompat.startForegroundService(context, updateIntent);
    }

    public static class UpdateService extends Service {
        // all real work is done in a service to avoid ANR messages
        @Override
        public int onStartCommand(Intent intent, int flags, int startId) {
            Cat.d("UpdateService start command");
            // We won't take long, but still we need to keep display a notification while updating
            NotificationManager nm = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);
            if (nm == null) {
                Cat.e("We were unable to receive the notification manager.");
                return START_NOT_STICKY;
            }
            String channelId = "be.ppareit.swiftp.widget_provider.channelId";
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                CharSequence name = "Update's the notification";
                String description = "This notification checks if the FTP Server is running.";
                int importance = NotificationManager.IMPORTANCE_NONE;
                NotificationChannel channel = new NotificationChannel(channelId, name, importance);
                channel.setDescription(description);
                nm.createNotificationChannel(channel);
            }
            Notification notification = new NotificationCompat.Builder(this)
                    .setContentTitle("FTP Server")
                    .setContentText("Widget provider background service")
                    .setSmallIcon(R.mipmap.notification)
                    .setOngoing(true)
                    .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
                    .setCategory(NotificationCompat.CATEGORY_SERVICE)
                    .setPriority(NotificationCompat.PRIORITY_MIN)
                    .setShowWhen(false)
                    .setChannelId(channelId)
                    .build();
            if (Build.VERSION.SDK_INT >= 34) {
                startForeground(33, notification, ServiceInfo.FOREGROUND_SERVICE_TYPE_CONNECTED_DEVICE);
            } else {
                startForeground(33, notification);

            }
            // depending on whether or not the server is running, choose correct properties
            final String action;
            final int drawable;
            final String text;
            if (FsService.isRunning()) {
                action = FsService.ACTION_REQUEST_STOP;
                drawable = R.drawable.widget_on;
                // get ip address
                InetAddress address = FsService.getLocalInetAddress();
                if (address == null) {
                    Cat.w("Unable to retrieve the local ip address");
                    text = "ERROR";
                } else {
                    text = address.getHostAddress();
                }
            } else {
                action = FsService.ACTION_REQUEST_START;
                drawable = R.drawable.widget_off;
                text = getString(R.string.swiftp_name);
            }
            Intent startIntent = new Intent(App.getAppContext(), FsService.class);
            startIntent.setAction(action);
            PendingIntent pendingIntent;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                pendingIntent = PendingIntent.getForegroundService(this, 0,
                        startIntent, PendingIntent.FLAG_IMMUTABLE);
            } else {
                pendingIntent = PendingIntent.getService(this, 0,
                        startIntent, 0);
            }
            RemoteViews views = new RemoteViews(getPackageName(), R.layout.widget_layout);
            // setup the info on the widget
            views.setOnClickPendingIntent(R.id.widget_button, pendingIntent);
            views.setImageViewResource(R.id.widget_button, drawable);
            views.setTextViewText(R.id.widget_text, text);
            // new info is on widget, update it
            AppWidgetManager manager = AppWidgetManager.getInstance(this);
            ComponentName widget = new ComponentName(this, FsWidgetProvider.class);
            manager.updateAppWidget(widget, views);
            // service has done it's work, android may kill it
            stopSelf();
            return START_NOT_STICKY;
        }

        @Override
        public IBinder onBind(Intent intent) {
            return null;
        }

        @Override
        public void onDestroy() {
            super.onDestroy();
            stopForeground(true);
        }
    }

}
