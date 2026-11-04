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

import android.annotation.SuppressLint
import android.content.Context
import android.net.wifi.WifiManager
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.os.PowerManager
import android.util.Log
import kotlin.time.Duration.Companion.minutes

/**
 * The wake lock and Wi-Fi lock the server holds, following the battery saver setting.
 *
 * HIGH holds both for as long as the server runs. LOW takes them when a client connects
 * and releases them ten minutes after the last open session ends. DEEP never takes them.
 *
 * Every entry point is synchronized: sessions start on the listener thread, end on their
 * own threads, and the delayed release runs on the main thread.
 */
class ServerWakeLocks(
    context: Context,
    private val mode: Mode,
    private val fullWakeLock: Boolean,
) {

    enum class Mode {
        HIGH, LOW, DEEP;

        companion object {
            /** The `battery_saver` preference value: "0", "1" or "2". */
            @JvmStatic
            fun fromPreference(value: String?): Mode = when (value) {
                "0" -> HIGH
                "1" -> LOW
                else -> DEEP
            }
        }
    }

    private val powerManager =
        context.applicationContext.getSystemService(Context.POWER_SERVICE) as PowerManager
    private val wifiManager =
        context.applicationContext.getSystemService(Context.WIFI_SERVICE) as WifiManager
    private val handler = Handler(Looper.getMainLooper())
    private val delayedRelease = Runnable { releaseIfIdle() }

    private var wakeLock: PowerManager.WakeLock? = null
    private var wifiLock: WifiManager.WifiLock? = null
    private var openSessions = 0
    private var stopped = false

    val isHeld: Boolean
        @Synchronized get() = wakeLock != null

    /** The server is up. Only HIGH takes the locks now. */
    @Synchronized
    fun start() {
        if (stopped || mode != Mode.HIGH) return
        // @TODO: when using ethernet, is it needed to take wifi lock?
        acquire()
    }

    /** A client connected. In LOW this takes the locks and cancels a pending release. */
    @Synchronized
    fun sessionStarted() {
        if (stopped || mode != Mode.LOW) return
        openSessions++
        handler.removeCallbacks(delayedRelease)
        acquire()
    }

    /* Handle a delayed reaction to ending connection wakelocks.
     * As its not possible to know when it will end since ftp client can quit and connect right back
     * again, need to create a delayed reaction. A handler with a delayed message works well.
     * */
    @Synchronized
    fun sessionEnded() {
        if (stopped || mode != Mode.LOW) return
        if (openSessions > 0) openSessions--
        if (openSessions > 0) return
        handler.removeCallbacks(delayedRelease)
        handler.postDelayed(delayedRelease, RELEASE_DELAY.inWholeMilliseconds)
    }

    /** The listener stopped accepting. In LOW the connection locks go with it. */
    @Synchronized
    fun releaseConnectionLocks() {
        if (mode != Mode.LOW) return
        handler.removeCallbacks(delayedRelease)
        release()
    }

    /** The server is down. Releases everything; later calls do nothing. */
    @Synchronized
    fun stop() {
        stopped = true
        handler.removeCallbacks(delayedRelease)
        openSessions = 0
        release()
    }

    @Synchronized
    private fun releaseIfIdle() {
        if (openSessions == 0) release()
    }

    /**
     * Takes the wake lock
     *
     * Many devices do not properly honor a PARTIAL_WAKE_LOCK. For these devices, we have an option
     * to force the phone into a full wake lock.
     */
    @SuppressLint("WakelockTimeout") // held for as long as the server serves, no timeout to give
    private fun acquire() {
        if (wakeLock == null) {
            wakeLock = if (fullWakeLock) {
                Log.d(TAG, "acquire: Taking full wake lock")
                // Note: FULL_WAKE_LOCK is deprecated, officially not recommended, and is actually worse.
                @Suppress("DEPRECATION")
                powerManager.newWakeLock(PowerManager.FULL_WAKE_LOCK, LOCK_TAG)
            } else {
                Log.d(TAG, "acquire: Taking partial wake lock")
                powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, LOCK_TAG)
            }.apply {
                setReferenceCounted(false)
                acquire()
            }
        }
        if (wifiLock == null) {
            Log.d(TAG, "acquire: Taking wifi lock")
            wifiLock = if (Build.VERSION.SDK_INT >= 29) {
                // Low is forced starting in Android 14 and starts use at Android 10.
                wifiManager.createWifiLock(WifiManager.WIFI_MODE_FULL_LOW_LATENCY, LOCK_TAG)
            } else {
                @Suppress("DEPRECATION")
                wifiManager.createWifiLock(LOCK_TAG)
            }.apply {
                setReferenceCounted(false)
                acquire()
            }
        }
    }

    private fun release() {
        wifiLock?.let {
            Log.d(TAG, "release: Releasing wifi lock")
            it.release()
        }
        wifiLock = null
        wakeLock?.let {
            Log.d(TAG, "release: Releasing wake lock")
            it.release()
        }
        wakeLock = null
    }

    companion object {
        private const val TAG = "ServerWakeLocks"
        // Shown in battery stats and Android vitals.
        private const val LOCK_TAG = "SwiFTP:server"
        private val RELEASE_DELAY = 10.minutes
    }
}
