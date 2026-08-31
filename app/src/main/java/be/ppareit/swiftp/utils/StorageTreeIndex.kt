// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

/**
 * The set of granted folders.
 */
class StorageTreeIndex(granted: List<StorageTree>?) {

    val trees: List<StorageTree> = granted?.toList() ?: emptyList()

    fun isEmpty(): Boolean = trees.isEmpty()

    /**
     * The granted folder holding this path, or null when no folder does. When folders overlap
     * (a grant on Documents and another on Documents/Sub) the most specific one wins.
     */
    fun containing(path: String?): StorageTree? =
        trees.filter { it.contains(path) }.maxByOrNull { it.rootPath.length }

    /**
     * The granted folder a raw SAF document id belongs to, for example "primary:Documents/notes.txt"
     * belongs to a grant on "primary:Documents". Only for callers holding an id; a File path
     * is answered by [containing], also when the path holds a colon.
     */
    fun owningDocumentId(documentId: String?): StorageTree? =
        trees.filter { it.containsDocumentId(documentId) }.maxByOrNull { it.documentId.length }

    /**
     * True when this directory is above at least one granted folder without being inside one.
     */
    fun isVirtual(dir: String?): Boolean {
        if (dir == null || containing(dir) != null) return false
        return trees.any { it.rootPath.isStrictlyBelow(dir) }
    }

    /**
     * The entries to show for a virtual directory.
     */
    fun childNamesUnder(dir: String?): List<String> {
        if (dir == null) return emptyList()
        val prefixLength = dir.withTrailingSeparator().length
        return trees
            .filter { it.rootPath.isStrictlyBelow(dir) }
            .map { it.rootPath.substring(prefixLength).substringBefore('/') }
            .distinct()
    }

    /**
     * The chroot a user should get by default: the folder itself when only one is granted, and
     * otherwise the deepest directory holding all of them, which becomes the virtual root.
     * Null when nothing is granted.
     */
    fun defaultChroot(): String? =
        trees.map { it.rootPath }.reduceOrNull(::commonAncestor)
}

/** True when this path sits strictly below [parent], with the separator taking part. */
private fun String.isStrictlyBelow(parent: String): Boolean =
    length > parent.length && startsWith(parent.withTrailingSeparator())

private fun String.withTrailingSeparator(): String = if (endsWith("/")) this else "$this/"

/** The deepest directory containing both paths, cut on a separator boundary. */
private fun commonAncestor(a: String, b: String): String {
    val shared = a.split("/").zip(b.split("/"))
        .takeWhile { (left, right) -> left == right }
        .map { it.first }
    // The leading empty segment of an absolute path rejoins as the leading separator.
    return if (shared.size < 2) "/" else shared.joinToString("/")
}
