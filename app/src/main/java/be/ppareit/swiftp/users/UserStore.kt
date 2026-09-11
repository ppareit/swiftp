// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.users

import be.ppareit.swiftp.App
import be.ppareit.swiftp.FsSettings
import be.ppareit.swiftp.R
import be.ppareit.swiftp.Util
import be.ppareit.swiftp.server.AuthResult
import be.ppareit.swiftp.server.Authenticator
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

/**
 * The users the app stores, and the answer the server asks of them.
 *
 * Kept out of FsSettings so that the user model, the JSON that persists it and the
 * migrations it needs live in one place, and out of the server package so that the protocol
 * does not depend on how a user is stored.
 */
object UserStore : Authenticator {

    private const val USERS = "users"

    /**
     * The chroot of a user that is served the allowed folders, whichever they are.
     *
     * It is resolved at the login and never stored as a path, so changing the allowed folders
     * moves these users along instead of leaving them behind on a folder that was shared once.
     * A user restricted to one folder keeps that folder, see [isStranded] for what happens when
     * that folder goes away.
     */
    const val ALL_ALLOWED_FOLDERS = ""

    private val sp get() = FsSettings.preferences()

    /** Every stored user. Never empty. */
    fun users(): List<FtpUser> {
        val stored = stored()
        val users = stored.map(::normalized)
        // Written back rather than recomputed on every read: what [normalized] recognizes is a
        // path that is the whole served set, and that stops being recognizable the moment the
        // allowed folders change, which is exactly when it would be needed.
        if (users != stored) save(users)
        return users
    }

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
        return normalized(user)
    }

    /**
     * True when this user is restricted to a folder that is not served any more, so nothing
     * would be reachable after the login. Every screen showing users should say so.
     */
    fun isStranded(user: FtpUser): Boolean =
        user.chroot != ALL_ALLOWED_FOLDERS && !Util.isPathServed(user.chroot)

    /** The users in that state, empty when everybody can still be served. */
    fun strandedUsers(): List<FtpUser> = users().filter(::isStranded)

    /** Puts a stranded user back on the allowed folders, which always reach something. */
    fun serveAllAllowedFolders(username: String) {
        val user = user(username) ?: return
        modify(username, user.copy(chroot = ALL_ALLOWED_FOLDERS))
    }

    /**
     * The chroot the session starts in, or why it gets none.
     *
     * Comparing the password here rather than in the server is what lets it be stored
     * differently later without the protocol knowing.
     */
    override fun authenticate(username: String, password: String): AuthResult {
        val user = user(username) ?: return AuthResult.Refused
        if (user.password != password) return AuthResult.Refused
        if (user.chroot == ALL_ALLOWED_FOLDERS) {
            return AuthResult.Accepted(FsSettings.getDefaultChrootDir().path)
        }
        if (!Util.isPathServed(user.chroot)) return AuthResult.FolderNotShared
        return AuthResult.Accepted(user.chroot)
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
                    sp.getString("chrootDir", null) ?: ALL_ALLOWED_FOLDERS,
                )
            )
        }
        return listOf( // Nothing in store
            FtpUser(
                context.getString(R.string.username_default),
                context.getString(R.string.password_default),
                ALL_ALLOWED_FOLDERS,
            )
        )
    }

    private fun save(users: List<FtpUser>) {
        sp.edit().putString(USERS, Gson().toJson(users)).apply()
    }

    /**
     * A stored chroot that is exactly the folder set the app serves anyway reads back as
     * [ALL_ALLOWED_FOLDERS].
     *
     * Older versions had no way to say "the allowed folders": the user list showed the resolved
     * path and stored it back on the next edit, so a chroot that was only ever the default is
     * frozen in the preferences. The two mean the same thing today, and this way they keep
     * meaning the same thing tomorrow. A folder the person actually picked is smaller than the
     * whole set, so it is left alone.
     */
    private fun normalized(user: FtpUser): FtpUser =
        if (user.chroot != ALL_ALLOWED_FOLDERS
            && user.chroot == FsSettings.getDefaultChrootDir().path
        ) user.copy(chroot = ALL_ALLOWED_FOLDERS)
        else user
}
