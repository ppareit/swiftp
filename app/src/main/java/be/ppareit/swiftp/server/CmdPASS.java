/*
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
 */

package be.ppareit.swiftp.server;

import android.util.Log;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.AllowedFolders;
import be.ppareit.swiftp.utils.AnonymousLimit;
import be.ppareit.swiftp.utils.IPSecurity;

public class CmdPASS extends FtpCmd implements Runnable {
    private static final String TAG = CmdPASS.class.getSimpleName();

    String input;

    public CmdPASS(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "Executing PASS");
        String attemptPassword = getParameter(input, true); // silent
        // Always first USER command, then PASS command
        String attemptUsername = sessionThread.getUserName();
        if (attemptUsername == null) {
            sessionThread.writeString("503 Must send USER first\r\n");
            return;
        }
        if (attemptUsername.equals("anonymous") && settings().allowAnonymous()) {
            final int anonMaxCon = settings().getAnonMaxConNumber();
            final int newCount = AnonymousLimit.incrementAndGet();
            logging().appendLog("anon CURRENT client conn count: " + (newCount - 1));
            logging().appendLog("anon MAX conn count: " + anonMaxCon);
            if (newCount > anonMaxCon) {
                Log.i(TAG, "Failed authentication, too many anonymous users connected.");
                Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
                sessionThread.writeString("421 too many anonymous users connected.\r\n");
                sessionThread.authAttempt(false);
            } else if (nothingIsShared()) {
                refuseNothingShared();
            } else {
                // An empty chroot is "the allowed folders", the session already starts there
                final String anonChroot = settings().getAnonChroot();
                if (!anonChroot.isEmpty() && !settings().isPathServed(anonChroot)) {
                    refuseFolderNotShared("anonymous");
                    return;
                }
                Log.i(TAG, "Guest logged in with email: " + attemptPassword);
                sessionThread.writeString("230 Guest login ok, read only access.\r\n");
                if (!anonChroot.isEmpty()) {
                    sessionThread.setChrootDir(anonChroot);
                }
            }
            return;
        }
        final AuthResult result = authenticator().authenticate(attemptUsername, attemptPassword);
        if (result instanceof AuthResult.Refused) {
            Log.i(TAG, "Failed authentication");
            Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
            sessionThread.writeString("530 Login incorrect.\r\n");
            sessionThread.authAttempt(false);
            IPSecurity.putIPFail(sessionThread.getRemoteAddress());
            return;
        }
        if (nothingIsShared()) {
            refuseNothingShared();
            return;
        }
        if (result instanceof AuthResult.FolderNotShared) {
            refuseFolderNotShared(attemptUsername);
            return;
        }
        Log.i(TAG, "User " + attemptUsername + " password verified");
        sessionThread.writeString("230 Access granted\r\n");
        sessionThread.authAttempt(true);
        sessionThread.setChrootDir(((AuthResult.Accepted) result).getChroot());
    }

    /**
     * Nothing can be served if there are no folders shared.
     */
    private boolean nothingIsShared() {
        return Util.useScopedStorage() && AllowedFolders.isEmpty();
    }

    /**
     * We could refuse already in SessionThread, for instance instead of the welcome message. But
     * after PASS ensures we dont leak information about the servers state to world.
     */
    private void refuseNothingShared() {
        Log.i(TAG, "Refusing login, no folders are shared.");
        // Tell client what happened
        sessionThread.writeString("421 " + ServerProblem.NOTHING_SHARED.message() + "\r\n");
        sessionThread.authAttempt(false);
        // Tell server what happened, this will show up in the settings screen
        FsService.reportProblem(ServerProblem.NOTHING_SHARED);
        // 421 is "closing control connection", and authAttempt only counts the failure. Without
        // the close the refusal is advisory: isAnonymouslyLoggedIn() reads the global setting,
        // not the session, so a refused guest could go on to LIST and RETR.
        sessionThread.quit();
    }

    /**
     * The credentials were right, the folder this login is kept in is not shared. So we refuse
     * login and report as a problem, so the user sees what login is giving problem.
     */
    private void refuseFolderNotShared(String username) {
        Log.i(TAG, "Refusing login, the folder for this login is not shared.");
        sessionThread.writeString("421 " + ServerProblem.USER_FOLDER_NOT_SHARED.message(username)
                + "\r\n");
        sessionThread.authAttempt(false);
        FsService.reportProblem(ServerProblem.USER_FOLDER_NOT_SHARED, username);
        // Like a refused anonymous login: without the close the refusal would be advisory
        sessionThread.quit();
    }
}
