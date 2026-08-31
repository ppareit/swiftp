// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.users

import be.ppareit.swiftp.App
import be.ppareit.swiftp.FsSettings
import be.ppareit.swiftp.R
import be.ppareit.swiftp.server.Authenticator
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken
import java.io.File

/**
 * The users the app stores, and the answer the server asks of them.
 *
 * Kept out of FsSettings so that the user model, the JSON that persists it and the
 * migrations it needs live in one place, and out of the server package so that the protocol
 * does not depend on how a user is stored.
 */
object UserStore : Authenticator {

    private const val USERS = "users"

    private val sp get() = FsSettings.preferences()

    /** Every stored user, with a chroot that is a directory. Never empty. */
    fun users(): List<FtpUser> = stored().map(::withUsableChroot)

    /** The user with this name, or null when there is none. */
    fun user(username: String?): FtpUser? = users().firstOrNull { it.username == username }

    fun add(user: FtpUser) {
        require(user(user.username) == null) { "User already exists" }
        save(stored() + user)
    }

    fun remove(username: String) {
        save(stored().filterNot { it.username == username })
    }

    /** Returns the user as it reads back, which is not always the one handed in. */
    fun modify(username: String, user: FtpUser): FtpUser {
        remove(username)
        add(user)
        return withUsableChroot(user)
    }

    /**
     * The chroot the session starts in, or null when the credentials are refused.
     *
     * Comparing the password here rather than in the server is what lets it be stored
     * differently later without the protocol knowing.
     */
    override fun authenticate(username: String, password: String): String? {
        val user = user(username) ?: return null
        return if (user.password == password) user.chroot else null
    }

    /** What the preferences hold, migrated but not normalized. */
    private fun stored(): List<FtpUser> {
        val context = App.getAppContext()
        if (sp.contains(USERS)) {  // Default
            val listType = object : TypeToken<List<FtpUser>>() {}.type
            return Gson().fromJson(sp.getString(USERS, null), listType)
        }
        if (sp.contains("username")) { // Before 2.19 a single user was three loose preferences
            return listOf(
                FtpUser(
                    sp.getString("username", null) ?: context.getString(R.string.username_default),
                    sp.getString("password", null) ?: context.getString(R.string.password_default),
                    sp.getString("chrootDir", null) ?: "",
                )
            )
        }
        return listOf( // Nothing in store
            FtpUser(
                context.getString(R.string.username_default),
                context.getString(R.string.password_default),
                "",
            )
        )
    }

    private fun save(users: List<FtpUser>) {
        sp.edit().putString(USERS, Gson().toJson(users)).apply()
    }

    /**
     * A chroot that is not a directory serves nothing, so it reads back as the default.
     *
     * This used to be attempted in the user constructor, where Gson never reached it: it
     * allocates without calling one, so exactly the stored users skipped the check.
     */
    private fun withUsableChroot(user: FtpUser): FtpUser =
        if (File(user.chroot).isDirectory) user
        else user.copy(chroot = FsSettings.getDefaultChrootDir().path)
}
