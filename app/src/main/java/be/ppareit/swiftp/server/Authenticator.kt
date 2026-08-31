// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.server

/**
 * Where the host decides whether a login is accepted.
 *
 * Separate from [Settings], the server never learns how the host keeps its users.
 */
interface Authenticator {

    /** The chroot the session starts in, or null when the credentials are refused. */
    fun authenticate(username: String, password: String): String?
}
