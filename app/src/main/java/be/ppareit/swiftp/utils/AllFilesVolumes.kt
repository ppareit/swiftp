// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import be.ppareit.swiftp.Util

import java.io.File

/** The mounted, readable volumes served through direct File access. */
object AllFilesVolumes {

    const val VIRTUAL_ROOT = "/storage"

    /** A path is canonical, as [StorageRoots] hands them out. */
    data class Volume(val name: String, val path: String)

    @JvmStatic
    fun available(): List<Volume> {
        val primary = StorageRoots.primary()?.takeIf { it.isServable() } ?: return emptyList()
        val cards = StorageRoots.all()
            .filter { it != primary && it.parent == VIRTUAL_ROOT && it.isServable() }
            .map { Volume("SD-${it.name}", it.path) }
        return listOf(Volume("Internal", primary.path)) + cards
    }

    /** The volumes besides internal storage, the SD cards. */
    @JvmStatic
    fun cards(): List<Volume> = available().drop(1)

    @JvmStatic
    fun hasMultiple(): Boolean = available().size > 1

    @JvmStatic
    fun isVirtualRoot(path: String): Boolean = path == VIRTUAL_ROOT

    /** True under all files access for a chroot of /storage, where "/" lists the volumes by name. */
    @JvmStatic
    fun servesVirtualRoot(chroot: File): Boolean =
        !Util.useScopedStorage() && isVirtualRoot(chroot.path)

    @JvmStatic
    fun physicalPathForVirtual(path: String): String? {
        if (path == VIRTUAL_ROOT) return VIRTUAL_ROOT
        val mount = available().firstOrNull { path.isAtOrBelow("$VIRTUAL_ROOT/${it.name}") }
            ?: return null
        return mount.path + path.removePrefix("$VIRTUAL_ROOT/${mount.name}")
    }

    @JvmStatic
    fun virtualPathForPhysical(path: String): String? {
        if (path == VIRTUAL_ROOT) return VIRTUAL_ROOT
        val mount = available().firstOrNull { path.isAtOrBelow(it.path) } ?: return null
        return "$VIRTUAL_ROOT/${mount.name}" + path.removePrefix(mount.path)
    }

    @JvmStatic
    fun nameForPhysicalRoot(path: String): String? = available()
        .firstOrNull { path == it.path }?.name

    @JvmStatic
    fun containsPhysical(path: String): Boolean = available()
        .any { path.isAtOrBelow(it.path) }

    /** A directory File access can actually list, which is what serving it needs. */
    private fun File.isServable(): Boolean = isDirectory && list() != null

    private fun String.isAtOrBelow(root: String): Boolean = this == root || startsWith("$root/")
}
