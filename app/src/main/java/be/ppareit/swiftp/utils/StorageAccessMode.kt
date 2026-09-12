// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import androidx.preference.PreferenceManager
import androidx.core.content.edit

import be.ppareit.swiftp.App
import be.ppareit.swiftp.Util

/** The mutually exclusive way SwiFTP currently reaches shared storage. */
enum class StorageAccessMode {
    UNCHOSEN,
    ALL_FILES,
    SELECTED_FOLDERS,
}

/**
 * Keeps the user's choice separate from Android's grants. In particular, SAF grants may remain
 * saved while all-files access is active, but they are dormant until SELECTED_FOLDERS is chosen.
 */
object StorageAccessModeStore {

    @JvmStatic
    fun current(): StorageAccessMode {
        val prefs = prefs()
        if (prefs.contains(MODE_KEY)) {
            return runCatching {
                StorageAccessMode.valueOf(prefs.getString(MODE_KEY, null).orEmpty())
            }.getOrDefault(StorageAccessMode.UNCHOSEN)
        }

        // Preserve the effective behavior of installations upgraded from before modes existed.
        val migrated = when {
            AllowedFolders.hasSavedFolders() -> StorageAccessMode.SELECTED_FOLDERS
            StorageProbe.hasFullSdCardAccess() -> StorageAccessMode.ALL_FILES
            else -> StorageAccessMode.UNCHOSEN
        }
        prefs.edit { putString(MODE_KEY, migrated.name) }
        return migrated
    }

    @JvmStatic
    fun select(mode: StorageAccessMode) {
        if (current() == mode) return
        prefs().edit { putString(MODE_KEY, mode.name) }
        Util.resetScoped()
    }

    private fun prefs() = PreferenceManager.getDefaultSharedPreferences(App.getAppContext())

    private const val MODE_KEY = "storage_access_mode"
}
