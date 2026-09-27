// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.content.Context
import android.os.Build
import android.provider.DocumentsContract
import android.provider.DocumentsContract.Document
import android.util.Log

import androidx.documentfile.provider.DocumentFile

import java.io.File

/**
 * RNTO under selected folders: puts a file or folder at a new path, through SAF.
 *    - inside folder that is just rename
 *    - between folders on one volume the provider can move, from API 24 on
 *    - across volumes or below API 24, it is copy followed by delete
 */
object SafMove {

    private val TAG = SafMove::class.java.simpleName

    enum class Result { MOVED, TARGET_EXISTS, FAILED, SOURCE_KEPT }

    @JvmStatic
    fun move(from: File, to: File, context: Context): Result {
        val fromParent = from.parentFile ?: return Result.FAILED
        val toParent = to.parentFile ?: return Result.FAILED
        val source = AllowedFolders.documentAt(from.path)?.takeIf { it.exists() }
            ?: return Result.FAILED
        val sourceParent = AllowedFolders.documentAt(fromParent.path) ?: return Result.FAILED
        val targetParent = AllowedFolders.documentAt(toParent.path) ?: return Result.FAILED
        if (AllowedFolders.documentAt(to.path)?.exists() == true) return Result.TARGET_EXISTS

        val sameParent = StorageRoots.canonical(fromParent) == StorageRoots.canonical(toParent)
        val index = AllowedFolders.index()
        val sameVolume = index.containing(from.path)?.storageVolumeId() ==
                index.containing(to.path)?.storageVolumeId()
        // a move keeps the old name first, which must not land on another file
        val nameFree = from.name == to.name ||
                AllowedFolders.documentAt(File(toParent, from.name).path)?.exists() != true

        val result = when {
            sameParent -> {
                Log.d(TAG, "RNTO ${from.name} to ${to.path} by rename")
                if (rename(source, to.name, context)) Result.MOVED else Result.FAILED
            }
            sameVolume && nameFree && supportsMove(source, context) -> {
                Log.d(TAG, "RNTO ${from.name} to ${to.path} by move")
                moveWithinVolume(from, source, sourceParent, targetParent, to, context)
                    ?: copyThenDelete(from, source, targetParent, to.name, context)
            }
            else -> {
                Log.d(TAG, "RNTO ${from.name} to ${to.path} by copy")
                copyThenDelete(from, source, targetParent, to.name, context)
            }
        }
        // Android 9's provider renames and then throws, so what is on disk has the last word
        if (result == Result.FAILED && isAt(to) && !isAt(from)) return Result.MOVED
        return result
    }

    private fun isAt(file: File): Boolean = AllowedFolders.documentAt(file.path)?.exists() == true

    private fun rename(document: DocumentFile, name: String, context: Context): Boolean =
        runCatching {
            DocumentsContract.renameDocument(context.contentResolver, document.uri, name) != null
        }.getOrDefault(false)

    /** Null when the provider would not move, so the caller copies instead. */
    private fun moveWithinVolume(
        from: File,
        source: DocumentFile,
        sourceParent: DocumentFile,
        targetParent: DocumentFile,
        to: File,
        context: Context,
    ): Result? {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N) return null
        // the old name in the target folder, where a move puts it before any rename
        val movedTo = File(to.parentFile, from.name)
        val moved = runCatching {
            DocumentsContract.moveDocument(
                context.contentResolver, source.uri, sourceParent.uri, targetParent.uri,
            )
        }.onFailure { Log.i(TAG, "move failed: $it") }.getOrNull() != null
        if (!moved && !(isAt(movedTo) && !isAt(from))) return null
        if (from.name == to.name) return Result.MOVED
        // the moved document's URI lives under the source's tree, look it up in the target's
        val landed = AllowedFolders.documentAt(movedTo.path)
        return if (landed != null && rename(landed, to.name, context)) Result.MOVED else Result.FAILED
    }

    private fun copyThenDelete(
        from: File,
        source: DocumentFile,
        targetParent: DocumentFile,
        name: String,
        context: Context,
    ): Result {
        copy(from, source, targetParent, name, context) ?: return Result.FAILED
        return if (source.delete()) Result.MOVED else Result.SOURCE_KEPT
    }

    /** The copy, or null, and then nothing of it is left behind. */
    private fun copy(
        from: File,
        source: DocumentFile,
        targetParent: DocumentFile,
        name: String,
        context: Context,
    ): DocumentFile? {
        if (source.isDirectory) {
            // not listFiles(): it returns what it has when the query fails, and the source is
            // deleted afterwards, so a short listing would lose what it left out
            val names = childNames(source, context) ?: return null
            val dir = targetParent.createDirectory(name) ?: return null
            for (childName in names) {
                val childFrom = File(from, childName)
                val child = AllowedFolders.documentAt(childFrom.path)
                if (child == null || copy(childFrom, child, dir, childName, context) == null) {
                    dir.delete()
                    return null
                }
            }
            return dir
        }
        // the same type STOR creates files with, so the provider keeps the name as it is
        val file = targetParent.createFile("application/octet-stream", name) ?: return null
        val copied = runCatching {
            val resolver = context.contentResolver
            resolver.openInputStream(source.uri).use { input ->
                resolver.openOutputStream(file.uri).use { output ->
                    if (input == null || output == null) return@runCatching false
                    input.copyTo(output)
                }
            }
            true
        }.getOrDefault(false)
        if (!copied) {
            file.delete()
            return null
        }
        return file
    }

    /** The names of all children, or null when the provider could not list every one. */
    private fun childNames(dir: DocumentFile, context: Context): List<String>? = runCatching {
        val children = DocumentsContract.buildChildDocumentsUriUsingTree(
            dir.uri, DocumentsContract.getDocumentId(dir.uri),
        )
        context.contentResolver.query(
            children, arrayOf(Document.COLUMN_DISPLAY_NAME), null, null, null,
        )?.use { cursor ->
            List(cursor.count) { i ->
                check(cursor.moveToPosition(i))
                checkNotNull(cursor.getString(0))
            }
        }
    }.getOrNull()

    private fun supportsMove(document: DocumentFile, context: Context): Boolean = runCatching {
        context.contentResolver.query(
            document.uri, arrayOf(Document.COLUMN_FLAGS), null, null, null,
        )?.use { cursor ->
            cursor.moveToFirst() && cursor.getInt(0) and Document.FLAG_SUPPORTS_MOVE != 0
        } ?: false
    }.getOrDefault(false)
}
