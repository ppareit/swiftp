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
 * The wake lock and Wi-Fi lock the server holds, following the [KeepAwake] setting.
 *
 * ALWAYS holds both for as long as the server runs, and ALWAYS_SCREEN_ON adds a full wake
 * lock that keeps the screen on. WHILE_CONNECTED takes them when a client connects and
 * releases them ten minutes after the last open session ends. NEVER never takes them.
 *
 * Every entry point is synchronized: sessions start on the listener thread, end on their
 * own threads, and the delayed release runs on the main thread.
 */
class ServerWakeLocks(
    context: Context,
    private val mode: KeepAwake,
) {

    private val powerManager =
        context.applicationContext.getSystemService(Context.POWER_SERVICE) as PowerManager
    private val wifiManager =
        context.applicationContext.getSystemService(Context.WIFI_SERVICE) as WifiManager
    private val handler = Handler(Looper.getMainLooper())
    private val delayedRelease = Runnable { releaseIfIdle() }

    private var wakeLock: PowerManager.WakeLock? = null
    private var screenLock: PowerManager.WakeLock? = null
    private var wifiLock: WifiManager.WifiLock? = null
    private var openSessions = 0
    private var stopped = false

    val isHeld: Boolean
        @Synchronized get() = wakeLock != null

    val isScreenHeld: Boolean
        @Synchronized get() = screenLock != null

    private val holdsWhileConnected: Boolean
        get() = mode == KeepAwake.WHILE_CONNECTED

    /** The server is up. Only ALWAYS and ALWAYS_SCREEN_ON take the locks now. */
    @Synchronized
    fun start() {
        if (stopped) return
        if (mode != KeepAwake.ALWAYS && mode != KeepAwake.ALWAYS_SCREEN_ON) return
        // @TODO: when using ethernet, is it needed to take wifi lock?
        acquire()
    }

    /** A client connected. In WHILE_CONNECTED this takes the locks and cancels a pending release. */
    @Synchronized
    fun sessionStarted() {
        if (stopped || !holdsWhileConnected) return
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
        if (stopped || !holdsWhileConnected) return
        if (openSessions > 0) openSessions--
        if (openSessions > 0) return
        handler.removeCallbacks(delayedRelease)
        handler.postDelayed(delayedRelease, RELEASE_DELAY.inWholeMilliseconds)
    }

    /** The listener stopped accepting. In WHILE_CONNECTED the connection locks go with it. */
    @Synchronized
    fun releaseConnectionLocks() {
        if (!holdsWhileConnected) return
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
            Log.d(TAG, "acquire: Taking partial wake lock")
            wakeLock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, LOCK_TAG).apply {
                setReferenceCounted(false)
                acquire()
            }
        }
        // Next to the partial one, not instead: the power button releases a full wake lock
        // and lets the CPU sleep, a partial one keeps it running.
        if (mode == KeepAwake.ALWAYS_SCREEN_ON && screenLock == null) {
            Log.d(TAG, "acquire: Taking full wake lock")
            // Note: FULL_WAKE_LOCK is deprecated, officially not recommended, and is actually worse.
            @Suppress("DEPRECATION")
            screenLock = powerManager.newWakeLock(PowerManager.FULL_WAKE_LOCK, SCREEN_LOCK_TAG).apply {
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
        screenLock?.let {
            Log.d(TAG, "release: Releasing full wake lock")
            it.release()
        }
        screenLock = null
    }

    companion object {
        private const val TAG = "ServerWakeLocks"
        // Shown in battery stats and Android vitals.
        private const val LOCK_TAG = "SwiFTP:server"
        private const val SCREEN_LOCK_TAG = "SwiFTP:screen"
        private val RELEASE_DELAY = 10.minutes
    }
}
