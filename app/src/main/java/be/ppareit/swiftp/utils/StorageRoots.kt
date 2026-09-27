// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.os.Environment

import be.ppareit.swiftp.App

import java.io.File

/**
 * The mount points of the mounted storage volumes, we do NOT want to delete them.
 * Canonical, and the primary volume, when there is one, comes first.
 */
object StorageRoots {

    /** Internal storage, or null when Android reports none. */
    @JvmStatic
    fun primary(): File? = Environment.getExternalStorageDirectory()?.let { canonical(it) }

    @JvmStatic
    fun all(): List<File> {
        val roots = mutableListOf<File>()
        // ensure that the primary volume comes first
        primary()?.let { roots.add(it) }
        // The app's own folder on each volume is the only way to name removable mounts
        // before API 30. Everything from /Android/data/ on is the app's, the rest the volume.
        for (directory in App.getAppContext().getExternalFilesDirs(null)) {
            val path = directory?.absolutePath ?: continue
            val markerAt = path.indexOf("/Android/data/")
            if (markerAt > 0) roots.add(canonical(File(path.substring(0, markerAt))))
        }
        return roots.distinct()
    }

    /** True when the path is a volume root or a directory holding one, "/" and "/storage" */
    @JvmStatic
    fun isVolumeRootOrAbove(file: File): Boolean {
        val path = canonical(file).path
        if (path == File.separator) return true
        return all().any { it.path == path || it.path.startsWith(path + File.separator) }
    }

    /** Return the one canonical form of the file, . and .. resolved, symlinks followed*/
    @JvmStatic
    fun canonical(file: File): File =
        runCatching { file.canonicalFile }.getOrDefault(file.absoluteFile)
}
