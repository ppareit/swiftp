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

import android.app.Activity
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.view.WindowManager
import android.view.WindowManager.LayoutParams
import android.widget.Toast

import androidx.core.content.ContextCompat

import net.vrallev.android.cat.Cat

import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.R

class QuickToggleActivity : Activity() {

    private val handler = Handler(Looper.getMainLooper())
    private var feedbackReceiver: BroadcastReceiver? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // the window is invisible and it must allow tabs to the launcher behind it
        window.addFlags(LayoutParams.FLAG_NOT_TOUCHABLE or LayoutParams.FLAG_NOT_FOCUSABLE)

        // with nothing on screen to report the new state the toggle has to do it itself,
        // and Android only lets a notification-less app toast while it is in the foreground,
        // so this activity waits for the answer instead of leaving right away
        val reportResult = !FsNotification.isVisible(this)
        if (reportResult) listenForServerState()

        if (FsService.isRunning()) {
            FsService.stop()
        } else {
            FsService.start()
        }

        if (reportResult) {
            handler.postDelayed({
                Cat.w("No server state within ${FEEDBACK_TIMEOUT_MS}ms, leaving silently")
                leave()
            }, FEEDBACK_TIMEOUT_MS)
        } else {
            leave()
        }
    }

    /**
     * The server starts asynchronously and can still fail, so use broadcast for message
     */
    private fun listenForServerState() {
        val receiver = object : BroadcastReceiver() {
            override fun onReceive(context: Context, intent: Intent) {
                when (intent.action ?: return) {
                    FsService.ACTION_STARTED -> toast(
                        getString(
                            R.string.toast_server_started,
                            FsNotification.getServerAddressText()
                        ),
                        Toast.LENGTH_LONG
                    )

                    FsService.ACTION_STOPPED ->
                        toast(getString(R.string.toast_server_stopped), Toast.LENGTH_SHORT)

                    FsService.ACTION_FAILEDTOSTART ->
                        toast(getString(failureMessage(intent)), Toast.LENGTH_LONG)
                }
                leave()
            }
        }
        feedbackReceiver = receiver
        val filter = IntentFilter()
        filter.addAction(FsService.ACTION_STARTED)
        filter.addAction(FsService.ACTION_STOPPED)
        filter.addAction(FsService.ACTION_FAILEDTOSTART)
        // stuck on ContextCompat till API >= 33 because RECEIVER_NOT_EXPORTED
        ContextCompat.registerReceiver(
            this, receiver, filter,
            ContextCompat.RECEIVER_NOT_EXPORTED
        )
    }

    private fun toast(text: CharSequence, duration: Int) {
        Toast.makeText(this, text, duration).show()
    }

    /** Which of the three failures it was; "failed to start" alone reads like a bug in the app. */
    private fun failureMessage(intent: Intent) =
        when (intent.getIntExtra(FsService.EXTRA_FAILURE, FsService.FAILURE_NO_NETWORK)) {
            FsService.FAILURE_MOBILE_ONLY -> R.string.running_summary_failed_mobile_only
            FsService.FAILURE_PORT -> R.string.running_summary_failed_port
            else -> R.string.running_summary_failed_no_network
        }

    /**
     * Back to the launcher, the toggle never has anything of its own to show.
     */
    private fun leave() {
        handler.removeCallbacksAndMessages(null)
        stopListening()
        startActivity(
            Intent(Intent.ACTION_MAIN)
                .addCategory(Intent.CATEGORY_HOME)
                .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        )
        finish()
    }

    private fun stopListening() {
        val receiver = feedbackReceiver ?: return
        unregisterReceiver(receiver)
        feedbackReceiver = null
    }

    override fun onDestroy() {
        super.onDestroy()
        handler.removeCallbacksAndMessages(null)
        stopListening()
    }

    companion object {

        const val ACTION_QUICK_TOGGLE = "be.ppareit.swiftp.action.QUICK_TOGGLE"

        private const val FEEDBACK_TIMEOUT_MS = 5000L
    }
}
