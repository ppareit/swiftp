package be.ppareit.swiftp.utils

import android.os.Environment
import android.util.Log

import java.io.File

/**
 * Answers one question: can the plain File path still serve the whole of shared storage?
 *
 * It is measured rather than deduced from the Android version, because neither the version nor
 * the LEGACY_STORAGE key is reliable. Some ROMs, gaming handhelds above all, leave the legacy
 * behavior on well past Android 11, or do not mount /storage/emulated through FUSE at all.
 */
object StorageProbe {

    private val TAG = StorageProbe::class.java.simpleName

    /** The measurement itself, swapped out in tests so both kinds of device can be covered. */
    fun interface Probe {
        fun hasFullSdCardAccess(): Boolean
    }

    private const val PROBE_FILENAME = ".swiftp-access-probe"

    @Volatile
    private var probe: Probe = RealProbe

    /** null until the device has been asked. Dropped whenever the granted folders change. */
    @Volatile
    private var cached: Boolean? = null

    @JvmStatic
    fun hasFullSdCardAccess(): Boolean {
        cached?.let { return it }
        val full = probe.hasFullSdCardAccess()
        cached = full
        Log.i(TAG, "Full sd card access probe: $full")
        return full
    }

    @JvmStatic
    fun invalidate() {
        cached = null
    }

    private object RealProbe : Probe {
        override fun hasFullSdCardAccess(): Boolean {
            if (Environment.getExternalStorageState() != Environment.MEDIA_MOUNTED) return false
            val root = Environment.getExternalStorageDirectory() ?: return false
            // From API 33 READ_EXTERNAL_STORAGE is never granted, so this already fails.
            if (root.list() == null) return false
            // On API 30 to 32 listing succeeds while opening a non-media file does not, so the
            // write half is what catches those.
            return FileUtil.isWritable(File(root, PROBE_FILENAME))
        }
    }

    @JvmStatic
    fun setProbeForTest(replacement: Probe?) {
        probe = replacement ?: RealProbe
        invalidate()
    }

    @JvmStatic
    fun resetProbeForTest() = setProbeForTest(null)
}
