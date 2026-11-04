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

import android.content.SharedPreferences

/**
 * When the server keeps the device awake, chosen by the user in the settings.
 *
 * Replaces two older preferences, the `battery_saver` list (HIGH, LOW, DEEP) and the
 * `stayAwake` checkbox, which only meant something in combination. Nothing of them is
 * carried over: every install starts at [NEVER], see [LegacyCleanup].
 */
enum class KeepAwake(val value: String) {
    /** Never; an incoming connection wakes the device by itself. */
    NEVER("never"),

    /** While a client is connected, and ten minutes after the last one leaves. */
    WHILE_CONNECTED("while_connected"),

    /** From start to stop. */
    ALWAYS("always"),

    /** From start to stop, and the screen stays on until the power button is pressed. */
    ALWAYS_SCREEN_ON("always_screen_on");

    companion object {
        const val KEY = "keep_awake"

        @JvmField
        val DEFAULT = NEVER

        @JvmStatic
        fun fromValue(value: String?): KeepAwake = entries.firstOrNull { it.value == value } ?: DEFAULT

        @JvmStatic
        fun read(prefs: SharedPreferences): KeepAwake = fromValue(prefs.getString(KEY, null))
    }
}
