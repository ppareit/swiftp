// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

/**
 * One granted ExternalStorageProvider folder, represented by its provider id and physical mount
 * path. The FTP-only aliases for these paths belong to [StorageTreeIndex], not to this object.
 */
class StorageTree private constructor(
    val documentId: String,
    val volumeId: String,
    val relativePath: String,
    /** The physical path of the granted folder, without a trailing separator. */
    val rootPath: String,
) {

    /** The folder name as shown to the user, eg "Documents". */
    val name: String
        get() = rootPath.substringAfterLast('/')

    /** What anything inside this folder starts with: "/storage/emulated/0/Documents/". */
    private val pathPrefix = "$rootPath/"

    /**
     * What the document id of anything inside this folder starts with: "primary:Documents/".
     * A volume-root grant is already "primary:", which is its own separator.
     */
    private val idPrefix = if (documentId.endsWith(":")) documentId else "$documentId/"

    /**
     * True when the path is this folder or something below it.
     */
    fun contains(filePath: String?): Boolean {
        val path = filePath?.trimTrailingSeparator() ?: return false
        return path == rootPath || path.startsWith(pathPrefix)
    }

    /** The same question on the document id side, for callers that hold an id rather than a path. */
    fun containsDocumentId(id: String?): Boolean =
        id != null && (id == documentId || id.startsWith(idPrefix))

    /**
     * The SAF document id for a path inside this folder, or null when the path is outside it.
     * This doubles as the membership test.
     */
    fun documentIdFor(filePath: String?): String? {
        if (!contains(filePath)) return null
        val path = filePath!!.trimTrailingSeparator()
        if (path == rootPath) return documentId
        return idPrefix + path.substring(pathPrefix.length)
    }

    /** Path below the volume root, including Documents for the historical home root. */
    fun pathWithinVolume(): String = when (volumeId) {
        HOME_ROOT_ID -> DOCUMENTS_DIR.joinRelative(relativePath)
        else -> relativePath
    }

    /** Home is another view of the primary volume, not a separate storage volume. */
    fun storageVolumeId(): String = if (volumeId == HOME_ROOT_ID) PRIMARY_ROOT_ID else volumeId

    override fun toString(): String = rootPath

    companion object {
        const val PRIMARY_ROOT_ID = "primary"

        /**
         * The root id of the "Documents" root, which the picker offers beside the volumes. It is
         * not a volume, so it is not mounted under /storage; it is the primary volume's Documents
         * folder. Everything else the provider publishes is a volume: "primary", or an uuid.
         */
        const val HOME_ROOT_ID = "home"

        /**
         * Where the "home" root points. Spelled out rather than taken from
         * Environment.DIRECTORY_DOCUMENTS, which keeps this class free of Android.
         */
        private const val DOCUMENTS_DIR = "Documents"

        /**
         * Builds a tree from an ExternalStorageProvider document id such as
         * "primary:Documents", "1A2B-3C4D:Music" or "home:". [volumeRoot] resolves a provider
         * volume id to its current mount path.
         *
         * Returns null for unknown volumes and non-canonical or malformed ids. In particular,
         * relative paths containing empty, current-directory or parent-directory segments are
         * rejected instead of being allowed to escape the resolved root.
         */
        fun fromDocumentId(
            documentId: String?,
            volumeRoot: (volumeId: String) -> String?,
        ): StorageTree? {
            if (documentId.isNullOrEmpty() || documentId.indexOf('\u0000') >= 0) return null
            val colon = documentId.indexOf(':')
            if (colon <= 0) return null

            val volume = documentId.substring(0, colon)
            if (volume.indexOf('/') >= 0) return null

            val relative = documentId.substring(colon + 1)
            if (!isCanonicalRelativePath(relative)) return null

            val volumeRootPath = when (volume) {
                HOME_ROOT_ID -> volumeRoot(PRIMARY_ROOT_ID)?.joinPath(DOCUMENTS_DIR)
                else -> volumeRoot(volume)
            }?.trimTrailingSeparator()?.takeIf { it.startsWith('/') } ?: return null

            return StorageTree(
                documentId = documentId,
                volumeId = volume,
                relativePath = relative,
                rootPath = volumeRootPath.joinPath(relative),
            )
        }

        private fun isCanonicalRelativePath(relative: String): Boolean {
            if (relative.startsWith('/') || relative.endsWith('/')) return false
            if (relative.isEmpty()) return true
            return relative.split('/').none { it.isEmpty() || it == "." || it == ".." }
        }
    }
}

private fun String.joinRelative(relative: String): String = when {
    isEmpty() -> relative
    relative.isEmpty() -> this
    else -> "$this/$relative"
}

private fun String.joinPath(relative: String): String {
    val base = trimTrailingSeparator()
    return when {
        relative.isEmpty() -> base
        base == "/" -> "/$relative"
        else -> "$base/$relative"
    }
}

private fun String.trimTrailingSeparator(): String {
    var end = length
    while (end > 1 && this[end - 1] == '/') end--
    return substring(0, end)
}
