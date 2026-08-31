// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.server

import java.io.File

/**
 * Configuration supplied to the FTP server by its host application.
 *
 * The server only depends on this platform-neutral contract. A host can back it with Android
 * preferences, while a JVM test can provide its own implementation without loading Android
 * classes or resetting global state.
 */
interface Settings {

    /** FTPS: whether explicit TLS is offered at all, ie whether AUTH SSL/TLS is answered. */
    fun useSSL(): Boolean

    /** Whether the plain port is refused, so only FTPS implicit and explicit are served. */
    fun isEncryptionOnlyEnabled(): Boolean

    /** Whether the FTPS implicit port is listened on. */
    fun isImplicitUsed(): Boolean

    /** Whether the plain port is skipped, leaving the implicit port as the only listener. */
    fun isImplicitOnly(): Boolean

    /** Whether FEAT is refused rather than answered with the feature list. */
    fun isFeatDisabled(): Boolean

    /** Whether SYST is refused rather than naming the system type. */
    fun isSystDisabled(): Boolean

    /** Whether the welcome banner is suppressed on connect. */
    fun isBannerDisabled(): Boolean

    /** Whether "anonymous" may log in without a password. */
    fun allowAnonymous(): Boolean

    /** How many anonymous sessions may be open at once. */
    fun getAnonMaxConNumber(): Int

    /** The chroot for anonymous, which is its own setting and not one of the users. */
    fun getAnonChroot(): String?

    /** The chroot a session starts in when the user has none of its own. */
    fun getDefaultChrootDir(): File

    /** Low end of the PASV data port range, 0 when unset. */
    fun getPortRangeLow(): Int

    /** High end of the PASV data port range, 0 when unset. */
    fun getPortRangeHigh(): Int

    /** Whether connections are written to the connection log. */
    fun isLoggingEnabled(): Boolean
}
