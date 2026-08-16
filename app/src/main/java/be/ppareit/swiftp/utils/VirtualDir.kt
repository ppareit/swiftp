package be.ppareit.swiftp.utils

/**
 * A directory that exists only in the listing.
 *
 * When several folders are allowed, say Documents and DCIM, we use VirtualDir to hold them.
 * These are listable and enterable but never readable or writable: the real files always live
 * inside a granted folder. [FileUtil.Gen] encodes that as canRead true, canWrite false.
 */
class VirtualDir(
    val path: String,
    val lastModified: Long,
) {
    val name: String
        get() = path.substringAfterLast('/')

    override fun toString(): String = path
}
