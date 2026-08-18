// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

/**
 * One granted SAF folder, reduced to the two strings that matter: the tree's document id
 * ("primary:Documents") and the [java.io.File] path it corresponds to
 * ("/storage/emulated/0/Documents").
 */
class StorageTree private constructor(
    val documentId: String,
    /** The File path of the granted folder, without a trailing separator. */
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

    override fun toString(): String = rootPath

    companion object {
        /**
         * Where the "primary" volume is mounted. Wrong for a secondary Android user, who gets
         * /storage/emulated/10 and up, so if we ever need to fix this, here it is.
         */
        private const val PRIMARY_ROOT = "/storage/emulated/0"

        /**
         * Builds a tree from a SAF tree document id such as "primary:Documents" or
         * "1A2B-3C4D:Music". Returns null when the id carries no volume separator, which means
         * it is not a tree id at all.
         */
        @JvmStatic
        fun fromDocumentId(documentId: String?): StorageTree? {
            if (documentId == null) return null
            val colon = documentId.indexOf(':')
            if (colon < 0) return null

            val volume = documentId.substring(0, colon)
            val relative = documentId.substring(colon + 1).trim('/')
            val volumeRoot = if (volume == "primary") PRIMARY_ROOT else "/storage/$volume"

            val root = if (relative.isEmpty()) volumeRoot else "$volumeRoot/$relative"
            return StorageTree(documentId, root)
        }
    }
}

private fun String.trimTrailingSeparator(): String {
    var end = length
    while (end > 1 && this[end - 1] == '/') end--
    return substring(0, end)
}
