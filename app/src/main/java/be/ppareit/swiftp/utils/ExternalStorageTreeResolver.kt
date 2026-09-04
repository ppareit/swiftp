// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.content.Context
import android.net.Uri
import android.os.Build
import android.os.Environment
import android.os.storage.StorageManager
import android.provider.DocumentsContract

import java.io.File

/** Adapts Android's ExternalStorageProvider tree URIs to [StorageTree]. */
object ExternalStorageTreeResolver {

    private const val AUTHORITY = "com.android.externalstorage.documents"
    private val APP_DATA_MARKER = "${File.separator}Android${File.separator}data${File.separator}"

    fun fromTreeUri(context: Context, treeUri: Uri?): StorageTree? {
        if (treeUri?.scheme != "content" || treeUri.authority != AUTHORITY) return null
        val segments = treeUri.pathSegments
        if (segments.size < 2 || segments[0] != "tree") return null

        val documentId = try {
            DocumentsContract.getTreeDocumentId(treeUri)
        } catch (_: IllegalArgumentException) {
            return null
        }

        val roots = volumeRoots(context)
        return StorageTree.fromDocumentId(documentId) { volumeId ->
            roots[volumeId] ?: roots.entries.firstOrNull {
                it.key.equals(volumeId, ignoreCase = true)
            }?.value
        }
    }

    private fun volumeRoots(context: Context): Map<String, String> {
        val roots = LinkedHashMap<String, String>()
        @Suppress("DEPRECATION")
        roots[StorageTree.PRIMARY_ROOT_ID] = Environment.getExternalStorageDirectory().absolutePath

        val appSpecificRoots = context.getExternalFilesDirs(null)
            .mapNotNull(::sharedStorageRoot)
        for (root in appSpecificRoots) {
            if (root != roots[StorageTree.PRIMARY_ROOT_ID]) {
                val volumeId = File(root).name
                if (!roots.containsKey(volumeId)) roots[volumeId] = root
            }
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            val storageManager = context.getSystemService(Context.STORAGE_SERVICE) as? StorageManager
            for (volume in storageManager?.storageVolumes.orEmpty()) {
                val volumeId = when {
                    volume.isPrimary -> StorageTree.PRIMARY_ROOT_ID
                    volume.uuid != null -> volume.uuid!!
                    else -> continue
                }
                val directory = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    volume.directory?.absolutePath
                } else {
                    appSpecificRoots.firstOrNull {
                        File(it).name.equals(volumeId, ignoreCase = true)
                    }
                }
                if (directory != null) roots[volumeId] = directory
            }
        }
        return roots
    }

    private fun sharedStorageRoot(appSpecificDir: File?): String? {
        val path = appSpecificDir?.absolutePath ?: return null
        val marker = path.indexOf(APP_DATA_MARKER)
        return if (marker > 0) path.substring(0, marker) else null
    }
}
