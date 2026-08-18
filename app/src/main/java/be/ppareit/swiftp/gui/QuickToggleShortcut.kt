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
package be.ppareit.swiftp.gui

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent

import androidx.core.content.pm.ShortcutInfoCompat
import androidx.core.content.pm.ShortcutManagerCompat
import androidx.core.graphics.drawable.IconCompat

import net.vrallev.android.cat.Cat

import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.R

/**
 * The shortcut shown when the launcher icon is long pressed. Label and icon follow server state
 */
class QuickToggleShortcut : BroadcastReceiver() {

    override fun onReceive(context: Context, intent: Intent) {
        val action = intent.action ?: return
        update(context, FsService.ACTION_STARTED == action)
    }

    companion object {

        private const val SHORTCUT_ID = "quick_toggle"

        /**
         * Publishes the shortcut. Never throw as this would take the whole app down
         */
        @JvmStatic
        fun update(context: Context, running: Boolean) {
            val appContext = context.applicationContext
            val shortcut = ShortcutInfoCompat.Builder(appContext, SHORTCUT_ID)
                .setShortLabel(
                    appContext.getString(
                        if (running) R.string.shortcut_stop_short_label
                        else R.string.shortcut_start_short_label
                    )
                )
                .setLongLabel(
                    appContext.getString(
                        if (running) R.string.shortcut_stop_long_label
                        else R.string.shortcut_start_long_label
                    )
                )
                .setIcon(
                    IconCompat.createWithResource(
                        appContext,
                        if (running) R.drawable.ic_shortcut_toggle_on
                        else R.drawable.ic_shortcut_toggle_off
                    )
                )
                .setIntent(
                    Intent(appContext, QuickToggleActivity::class.java)
                        .setAction(QuickToggleActivity.ACTION_QUICK_TOGGLE)
                )
                .build()
            try {
                ShortcutManagerCompat.setDynamicShortcuts(appContext, listOf(shortcut))
            } catch (e: RuntimeException) {
                Cat.e("Unable to publish the quick toggle shortcut: " + e.message)
            }
        }
    }
}
