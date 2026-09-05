// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

/**
 * The set of granted folders.
 */
class StorageTreeIndex(granted: List<StorageTree>?) {

    val trees: List<StorageTree> = granted?.toList() ?: emptyList()

    private data class Mount(
        val tree: StorageTree,
        val virtualRootPath: String,
    )

    private val physicalRoot = trees
        .map { it.rootPath }
        .reduceOrNull(::commonAncestor)
    private val storageVolumeCount = trees
        .map { it.storageVolumeId() }
        .distinct()
        .size
    private val mounts = trees.map { tree ->
        if (storageVolumeCount <= 1) {
            Mount(tree, tree.rootPath)
        } else {
            var relative = tree.pathWithinVolume()
            if (tree.storageVolumeId() != StorageTree.PRIMARY_ROOT_ID) {
                relative = tree.storageVolumeId().joinRelative(relative)
            } else if (relative.isEmpty()) {
                relative = StorageTree.PRIMARY_ROOT_ID
            }
            Mount(tree, physicalRoot?.joinPath(relative) ?: tree.rootPath)
        }
    }

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

    /** Resolves an FTP-only alias to the physical path used everywhere else in the server. */
    fun physicalPathForVirtual(path: String?): String? {
        val normalized = path?.trimTrailingSeparator() ?: return null
        val mount = mounts.filter { normalized.isAtOrBelow(it.virtualRootPath) }
            .maxByOrNull { it.virtualRootPath.length } ?: return null
        val suffix = normalized.substring(mount.virtualRootPath.length)
            .trimStart('/')
        return mount.tree.rootPath.joinPath(suffix)
    }

    /** Projects a physical path when the session uses the shared virtual root as its chroot. */
    fun virtualPathForPhysical(path: String?, chroot: String?): String? {
        val normalized = path?.trimTrailingSeparator() ?: return null
        if (chroot?.trimTrailingSeparator() != physicalRoot) return normalized
        if (normalized == physicalRoot) return physicalRoot
        val mount = mounts.filter { it.tree.contains(normalized) }
            .maxByOrNull { it.tree.rootPath.length } ?: return null
        val suffix = normalized.substring(mount.tree.rootPath.length)
            .trimStart('/')
        return mount.virtualRootPath.joinPath(suffix)
    }

    /**
     * True when this directory is above at least one granted folder without being inside one.
     */
    fun isVirtual(dir: String?): Boolean {
        val path = dir?.trimTrailingSeparator() ?: return false
        if (containing(path) != null || physicalPathForVirtual(path) != null) return false
        return mounts.any { it.virtualRootPath.isStrictlyBelow(path) }
    }

    /**
     * The entries to show for a virtual directory.
     */
    fun childNamesUnder(dir: String?): List<String> {
        if (dir == null) return emptyList()
        val path = dir.trimTrailingSeparator()
        val prefixLength = path.withTrailingSeparator().length
        return mounts
            .filter { it.virtualRootPath.isStrictlyBelow(path) }
            .map { it.virtualRootPath.substring(prefixLength).substringBefore('/') }
            .distinct()
    }

    /**
     * The chroot a user should get by default: the folder itself when only one is granted, and
     * otherwise the deepest directory holding all of them, which becomes the virtual root.
     * Null when nothing is granted.
     */
    fun defaultChroot(): String? = physicalRoot
}

private fun String.isAtOrBelow(parent: String): Boolean =
    this == parent || startsWith(parent.withTrailingSeparator())

/** True when this path sits strictly below [parent], with the separator taking part. */
private fun String.isStrictlyBelow(parent: String): Boolean =
    length > parent.length && startsWith(parent.withTrailingSeparator())

private fun String.withTrailingSeparator(): String = if (endsWith("/")) this else "$this/"

private fun String.trimTrailingSeparator(): String {
    var end = length
    while (end > 1 && this[end - 1] == '/') end--
    return substring(0, end)
}

private fun String.joinPath(relative: String): String {
    val base = trimTrailingSeparator()
    return when {
        relative.isEmpty() -> base
        base == "/" -> "/$relative"
        else -> "$base/$relative"
    }
}

private fun String.joinRelative(relative: String): String = when {
    isEmpty() -> relative
    relative.isEmpty() -> this
    else -> "$this/$relative"
}

/** The deepest directory containing both paths, cut on a separator boundary. */
private fun commonAncestor(a: String, b: String): String {
    val shared = a.split("/").zip(b.split("/"))
        .takeWhile { (left, right) -> left == right }
        .map { it.first }
    // The leading empty segment of an absolute path rejoins as the leading separator.
    return if (shared.size < 2) "/" else shared.joinToString("/")
}
