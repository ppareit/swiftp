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

import android.app.AlertDialog
import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.widget.Toast

import be.ppareit.swiftp.BuildConfig
import be.ppareit.swiftp.KeepAwake
import be.ppareit.swiftp.R

/**
 * Asks before keep device awake leaves never, and offers a GitHub issue when it does.
 */
object KeepAwakeReport {

    private const val NEW_ISSUE = "https://github.com/ppareit/swiftp/issues/new"

    /** Runs [change] only if the user goes ahead; dismissing the dialog keeps never. */
    @JvmStatic
    fun confirmLeavingNever(context: Context, mode: KeepAwake, change: Runnable) {
        AlertDialog.Builder(context)
            .setTitle(R.string.keep_awake_report_title)
            .setMessage(R.string.keep_awake_report_message)
            // Keep never is the positive button: stacked, it is the one on top
            .setPositiveButton(R.string.keep_awake_keep_never, null)
            .setNegativeButton(R.string.keep_awake_change_and_report) { _, _ ->
                change.run()
                open(context, mode)
            }
            .setNeutralButton(R.string.keep_awake_just_change) { _, _ -> change.run() }
            .show()
    }

    private fun open(context: Context, mode: KeepAwake) {
        val intent = Intent(Intent.ACTION_VIEW, issueUri(mode))
        try {
            context.startActivity(intent)
        } catch (e: ActivityNotFoundException) {
            Toast.makeText(context, R.string.keep_awake_report_no_browser, Toast.LENGTH_LONG).show()
        }
    }

    @JvmStatic
    fun issueUri(mode: KeepAwake): Uri = Uri.parse(NEW_ISSUE).buildUpon()
        .appendQueryParameter("title", "Keep device awake: Never did not work on ${device()}")
        .appendQueryParameter("body", issueBody(mode))
        .build()

    /** The same details, in the same form, as the feedback mail in [Menu]. */
    @JvmStatic
    fun issueBody(mode: KeepAwake): String = listOf(
        "Device: ${device()}",
        "Android version: ${Build.VERSION.RELEASE}-${Build.VERSION.SDK_INT}",
        "Application: ${BuildConfig.APPLICATION_ID} (${BuildConfig.FLAVOR})",
        "Application version: ${BuildConfig.VERSION_NAME} - ${BuildConfig.VERSION_CODE}",
        "Keep device awake: changed from never to ${mode.value}",
        "",
        "What happened with Never:",
        "",
    ).joinToString("\n")

    private fun device() = "${Build.MANUFACTURER} ${Build.MODEL}"
}
