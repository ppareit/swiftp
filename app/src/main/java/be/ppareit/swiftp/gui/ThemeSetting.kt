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

import android.app.UiModeManager
import android.content.Context
import android.os.Build

import androidx.appcompat.app.AppCompatDelegate
import androidx.preference.PreferenceManager

/**
 * Light, dark, or whatever the system uses, chosen by the user in the settings.
 */
object ThemeSetting {

    const val KEY = "theme"
    const val SYSTEM = "system"
    const val LIGHT = "light"
    const val DARK = "dark"

    /** Applies the saved choice, call it before the first activity starts. */
    @JvmStatic
    fun apply(context: Context) {
        val value = PreferenceManager.getDefaultSharedPreferences(context)
            .getString(KEY, SYSTEM) ?: SYSTEM
        apply(context, value)
    }

    /** Applies [value], running activities are recreated by AppCompat. */
    @JvmStatic
    fun apply(context: Context, value: String) {
        // On 31 and up, the system also needs to know, it draws the splash screen
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            context.getSystemService(UiModeManager::class.java)
                ?.setApplicationNightMode(toUiMode(value))
        }
        AppCompatDelegate.setDefaultNightMode(toNightMode(value))
    }

    @JvmStatic
    fun toNightMode(value: String): Int = when (value) {
        LIGHT -> AppCompatDelegate.MODE_NIGHT_NO
        DARK -> AppCompatDelegate.MODE_NIGHT_YES
        else -> AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM
    }

    @JvmStatic
    fun toUiMode(value: String): Int = when (value) {
        LIGHT -> UiModeManager.MODE_NIGHT_NO
        DARK -> UiModeManager.MODE_NIGHT_YES
        else -> UiModeManager.MODE_NIGHT_AUTO
    }
}
