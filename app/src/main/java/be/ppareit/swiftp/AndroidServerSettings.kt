// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp

import be.ppareit.swiftp.server.FtpUser
import be.ppareit.swiftp.server.Settings
import java.io.File

/** Android preference-backed settings supplied to the FTP server. */
object AndroidServerSettings : Settings {
    override fun useSSL(): Boolean = FsSettings.useSSL()
    override fun isEncryptionOnlyEnabled(): Boolean = FsSettings.isEncryptionOnlyEnabled()
    override fun isImplicitUsed(): Boolean = FsSettings.isImplicitUsed()
    override fun isImplicitOnly(): Boolean = FsSettings.isImplicitOnly()
    override fun isFeatDisabled(): Boolean = FsSettings.isFeatDisabled()
    override fun isSystDisabled(): Boolean = FsSettings.isSystDisabled()
    override fun isBannerDisabled(): Boolean = FsSettings.isBannerDisabled()
    override fun allowAnonymous(): Boolean = FsSettings.allowAnonymous()
    override fun getAnonMaxConNumber(): Int = FsSettings.getAnonMaxConNumber()
    override fun getAnonChroot(): String? = FsSettings.getAnonChroot()
    override fun getUser(username: String?): FtpUser? = FsSettings.getUser(username)
    override fun getDefaultChrootDir(): File = FsSettings.getDefaultChrootDir()
    override fun getPortRangeLow(): Int = FsSettings.getPortRangeLow()
    override fun getPortRangeHigh(): Int = FsSettings.getPortRangeHigh()
    override fun isLoggingEnabled(): Boolean = FsSettings.isLoggingEnabled()
}
