package be.ppareit.swiftp.utils

import android.Manifest
import android.content.Context
import android.content.pm.PackageManager
import android.os.Build

import androidx.core.content.ContextCompat
import androidx.preference.PreferenceManager

import be.ppareit.swiftp.App
import be.ppareit.swiftp.Util

/**
 * Handles the old READ/WRITE_EXTERNAL_STORAGE pair
 */
object LegacyStoragePermission {

    @JvmField
    val PERMISSIONS = arrayOf(
        Manifest.permission.READ_EXTERNAL_STORAGE,
        Manifest.permission.WRITE_EXTERNAL_STORAGE,
    )

    /**
     * Whether asking can achieve anything, API > 33 the permissions are never granted
     */
    @JvmStatic
    fun appliesHere(): Boolean = Build.VERSION.SDK_INT < Build.VERSION_CODES.TIRAMISU

    @JvmStatic
    fun isGranted(context: Context): Boolean = PERMISSIONS.all {
        ContextCompat.checkSelfPermission(context, it) == PackageManager.PERMISSION_GRANTED
    }

    /** Both that the permission is worth having here and that we do not have it. */
    @JvmStatic
    fun isMissing(context: Context): Boolean = appliesHere() && !isGranted(context)

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

    private const val REQUESTED_KEY = "legacy_storage_requested"

    private fun prefs() = PreferenceManager.getDefaultSharedPreferences(App.getAppContext())
}
