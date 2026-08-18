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

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.AllowedFolders;
import be.ppareit.swiftp.utils.AnonymousLimit;
import be.ppareit.swiftp.utils.IPSecurity;
import be.ppareit.swiftp.utils.Logging;

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
        if (attemptUsername.equals("anonymous") && FsSettings.allowAnonymous()) {
            final int anonMaxCon = FsSettings.getAnonMaxConNumber();
            Logging logging = new Logging();
            final int newCount = AnonymousLimit.incrementAndGet();
            logging.appendLog("anon CURRENT client conn count: " + (newCount - 1));
            logging.appendLog("anon MAX conn count: " + anonMaxCon);
            if (newCount > anonMaxCon) {
                Log.i(TAG, "Failed authentication, too many anonymous users connected.");
                Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
                sessionThread.writeString("421 too many anonymous users connected.\r\n");
                sessionThread.authAttempt(false);
            } else if (nothingIsShared()) {
                refuseNothingShared();
            } else {
                Log.i(TAG, "Guest logged in with email: " + attemptPassword);
                sessionThread.writeString("230 Guest login ok, read only access.\r\n");
                final String anonChroot = FsSettings.getAnonChroot();
                if (!anonChroot.isEmpty()) {
                    sessionThread.setChrootDir(anonChroot);
                }
            }
            return;
        }
        FtpUser user = FsSettings.getUser(attemptUsername);
        if (user == null) {
            Log.i(TAG, "Failed authentication, username does not exist!");
            Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
            sessionThread.writeString("500 Login incorrect.\r\n");
            sessionThread.authAttempt( false);
            IPSecurity.putIPFail(sessionThread.getRemoteAddress());
        } else if (user.getPassword().equals(attemptPassword)) {
            if (nothingIsShared()) {
                refuseNothingShared();
                return;
            }
            Log.i(TAG, "User " + user.getUsername() + " password verified");
            sessionThread.writeString("230 Access granted\r\n");
            sessionThread.authAttempt(true);
            sessionThread.setChrootDir(user.getChroot());
        } else {
            Log.i(TAG, "Failed authentication, incorrect password");
            Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
            sessionThread.writeString("530 Login incorrect.\r\n");
            sessionThread.authAttempt(false);
            IPSecurity.putIPFail(sessionThread.getRemoteAddress());
        }
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
        sessionThread.writeString("421 No folders are shared. Open SwiFTP on the device and go to Allowed folders.\r\n");
        sessionThread.authAttempt(false);
        // 421 is "closing control connection", and authAttempt only counts the failure. Without
        // the close the refusal is advisory: isAnonymouslyLoggedIn() reads the global setting,
        // not the session, so a refused guest could go on to LIST and RETR.
        sessionThread.quit();
    }
}
