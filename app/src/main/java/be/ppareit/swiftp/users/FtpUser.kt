// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.users

import com.google.gson.annotations.SerializedName

/**
 * One FTP user as the app stores and edits it.
 *
 * The server never sees this type: it asks [UserStore] whether a login is accepted and gets
 * back a chroot, nothing more.
 *
 * Every parameter has a default so that Kotlin emits a no-arg constructor. Gson then builds
 * instances through it rather than through Unsafe, and a key missing from stored JSON reads
 * as "" instead of leaving a null in a non-null field. The alternate names are the field
 * names an older version serialized.
 */
data class FtpUser(
    @SerializedName(value = "username", alternate = ["mUsername"])
    val username: String = "",
    @SerializedName(value = "password", alternate = ["mPassword"])
    val password: String = "",
    @SerializedName(value = "chroot", alternate = ["mChroot"])
    val chroot: String = "",
)
