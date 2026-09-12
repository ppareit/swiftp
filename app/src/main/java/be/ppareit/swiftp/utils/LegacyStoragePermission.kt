// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.Manifest
import android.annotation.SuppressLint
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import android.os.Environment
import android.provider.Settings

import androidx.core.content.ContextCompat
import androidx.preference.PreferenceManager

import be.ppareit.swiftp.App
import be.ppareit.swiftp.Util

/**
 * Handles direct shared-storage access: MANAGE_EXTERNAL_STORAGE on modern Android, and the old
 * READ/WRITE_EXTERNAL_STORAGE pair where those still work.
 */
object LegacyStoragePermission {

    @JvmField
    val PERMISSIONS = arrayOf(
        Manifest.permission.READ_EXTERNAL_STORAGE,
        Manifest.permission.WRITE_EXTERNAL_STORAGE,
    )

    @JvmStatic
    fun usesSettingsGrant(): Boolean = Build.VERSION.SDK_INT >= Build.VERSION_CODES.R

    @JvmStatic
    fun isGranted(context: Context): Boolean = if (usesSettingsGrant()) {
        Environment.isExternalStorageManager()
    } else {
        PERMISSIONS.all {
            ContextCompat.checkSelfPermission(context, it) == PackageManager.PERMISSION_GRANTED
        }
    }

    /**
     * The system shows its dialog once, after that requestPermissions is silently ignored!
     * shouldShowRequestPermissionRationale can not tell "never asked" from "denied for good",
     * both are false, hence the stored flag.
     */
    @JvmStatic
    fun wasRequested(): Boolean = prefs().getBoolean(REQUESTED_KEY, false)

    @JvmStatic
    fun markRequested() {
        prefs().edit().putBoolean(REQUESTED_KEY, true).apply()
    }

    @JvmStatic // empty when interaction was cancelled
    fun allGranted(results: IntArray): Boolean =
        results.isNotEmpty() && results.all { it == PackageManager.PERMISSION_GRANTED }

    /**
     * What a result means for the rest of the app: the dialog has now been seen, and a grant
     * changes which storage backend the server should use, so the measurement behind that
     * choice has to be taken again rather than waiting for a restart.
     *
     * @return whether everything asked for was granted.
     */
    @JvmStatic
    fun onResult(results: IntArray): Boolean {
        markRequested()
        val granted = allGranted(results)
        if (granted) Util.resetScoped()
        return granted
    }

    @JvmStatic
    @SuppressLint("InlinedApi") // Called only when usesSettingsGrant() has established API 30+.
    fun settingsIntent(context: Context): Intent = Intent(
        Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION,
        Uri.fromParts("package", context.packageName, null),
    )

    private const val REQUESTED_KEY = "legacy_storage_requested"

    private fun prefs() = PreferenceManager.getDefaultSharedPreferences(App.getAppContext())
}
