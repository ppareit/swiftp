/*
Copyright 2026 Pieter Pareit

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
package be.ppareit.swiftp

import android.content.Context

import androidx.core.app.NotificationManagerCompat

/**
 * Removes what older versions left behind on the device.
 */
object LegacyCleanup {

    @JvmStatic
    fun run(context: Context) {
        // Since 3.7.0: the widget no longer updates from a foreground service, so the notification
        // channel that service needed is gone. It was created on every server start, widget or not.
        NotificationManagerCompat.from(context)
            .deleteNotificationChannel("be.ppareit.swiftp.widget_provider.channelId")
    }
}
