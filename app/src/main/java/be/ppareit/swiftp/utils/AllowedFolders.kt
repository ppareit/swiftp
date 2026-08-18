// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.content.Context
import android.content.Intent
import android.net.Uri
import android.provider.DocumentsContract

import be.ppareit.swiftp.App
import be.ppareit.swiftp.Util

/**
 * The folders the user has allowed SwiFTP to serve.
 *
 * There is no preference behind this. Android already keeps the list, as the persisted URI
 * permissions the app holds, so that list is the single source of truth. The path arithmetic
 * lives in [StorageTree] and [StorageTreeIndex], which know nothing about Android. This object
 * is the only part that touches the ContentResolver.
 */
object AllowedFolders {

    /**
     * getPersistedUriPermissions is a binder call and the resolver runs once per listing entry,
     * so the parsed list is cached. The TTL is a backstop for a grant made or revoked outside
     * the app; every path inside the app invalidates explicitly.
     */
    private const val CACHE_TTL_MS = 10_000L

    fun interface Source {
        fun treeUris(): List<Uri>
    }

    private class Snapshot(
        val index: StorageTreeIndex,
        val uriByDocumentId: Map<String, Uri>,
        val builtAt: Long,
    )

    @Volatile
    private var source: Source = PersistedPermissions

    @Volatile
    private var cached: Snapshot? = null

    /** The granted folders, as the path questions the server needs to ask. */
    @JvmStatic
    fun index(): StorageTreeIndex = snapshot().index

    @JvmStatic
    fun all(): List<StorageTree> = index().trees

    @JvmStatic
    fun isEmpty(): Boolean = index().isEmpty()

    /** The granted folders as File paths, for showing to the user. */
    @JvmStatic
    fun paths(): List<String> = all().map { it.rootPath }

    /** The granted folder names, eg "Documents, DCIM" for the settings summary. */
    @JvmStatic
    fun names(): List<String> = all().map { it.name }

    /** The tree URI a folder came from, needed to build a document URI under it. */
    @JvmStatic
    fun uriFor(tree: StorageTree?): Uri? =
        tree?.let { snapshot().uriByDocumentId[it.documentId] }

    /** Takes a persistable grant on a folder the user picked. */
    @JvmStatic
    fun takeGrant(context: Context, treeUri: Uri?) {
        if (treeUri == null) return
        context.contentResolver.takePersistableUriPermission(
            treeUri,
            Intent.FLAG_GRANT_READ_URI_PERMISSION or Intent.FLAG_GRANT_WRITE_URI_PERMISSION,
        )
        invalidate()
    }

    /** Gives the grant on a folder back, so the server can no longer reach it. */
    @JvmStatic
    fun releaseGrant(context: Context, treeUri: Uri?) {
        if (treeUri == null) return
        context.contentResolver.releasePersistableUriPermission(
            treeUri,
            Intent.FLAG_GRANT_READ_URI_PERMISSION or Intent.FLAG_GRANT_WRITE_URI_PERMISSION,
        )
        invalidate()
    }

    /** Drops the cache, and the storage mode with it, since that is derived from this list. */
    @JvmStatic
    fun invalidate() {
        cached = null
        Util.resetScoped()
    }

    private fun snapshot(): Snapshot {
        val now = System.currentTimeMillis()
        cached?.let { if (now - it.builtAt < CACHE_TTL_MS) return it }
        return build(now).also { cached = it }
    }

    private fun build(now: Long): Snapshot {
        // A LinkedHashMap so the folders keep the order they were granted in, which is the order
        // the user sees in the settings summary and on the folder screen.
        val uris = LinkedHashMap<String, Uri>()
        val trees = mutableListOf<StorageTree>()
        for (uri in source.treeUris()) {
            val documentId = treeDocumentId(uri) ?: continue
            if (uris.containsKey(documentId)) continue
            val tree = StorageTree.fromDocumentId(documentId) ?: continue
            uris[documentId] = uri
            trees.add(tree)
        }
        return Snapshot(StorageTreeIndex(trees), uris, now)
    }

    /**
     * The tree document id of a tree URI, or null when the URI is not a tree at all. Written out
     * rather than using DocumentsContract.isTreeUri, which needs API 24 while the app still
     * supports 23.
     */
    private fun treeDocumentId(uri: Uri?): String? {
        val segments = uri?.pathSegments ?: return null
        if (segments.size < 2 || segments[0] != "tree") return null
        return segments[1]
    }

    /** The real source: whatever the system says this package still holds. */
    private object PersistedPermissions : Source {
        override fun treeUris(): List<Uri> {
            val context = App.getAppContext() ?: return emptyList()
            return context.contentResolver.persistedUriPermissions
                .filter { it != null && it.isReadPermission }
                .map { it.uri }
        }
    }

    @JvmStatic
    fun setSourceForTest(replacement: Source?) {
        source = replacement ?: PersistedPermissions
        invalidate()
    }

    @JvmStatic
    fun resetSourceForTest() = setSourceForTest(null)

    /** Builds a source from tree document ids, eg "primary:Documents". */
    @JvmStatic
    fun sourceOf(vararg treeDocumentIds: String): Source {
        val uris = treeDocumentIds.map {
            DocumentsContract.buildTreeDocumentUri(EXTERNAL_STORAGE_AUTHORITY, it)
        }
        return Source { uris }
    }

    private const val EXTERNAL_STORAGE_AUTHORITY = "com.android.externalstorage.documents"
}
