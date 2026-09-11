// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.server

/**
 * Where the host decides whether a login is accepted.
 *
 * Separate from [Settings], the server never learns how the host keeps its users.
 */
interface Authenticator {

    /** The chroot the session starts in, or why it does not get one. */
    fun authenticate(username: String, password: String): AuthResult
}

/**
 * What the host answers when a session offers credentials.
 */
sealed interface AuthResult {

    /** The credentials are accepted and the session starts in [chroot]. */
    data class Accepted(val chroot: String) : AuthResult

    /** No such user, or the wrong password. */
    object Refused : AuthResult

    /** The credentials are right, but the folder this user is restricted to is not served. */
    object FolderNotShared : AuthResult
}
