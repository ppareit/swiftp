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

import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.Util;
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
            SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
            final int anonMaxCon = Integer.parseInt(sp.getString("anon_max", "1"));
            Logging logging = new Logging();
            final int newCount = AnonymousLimit.incrementAndGet();
            logging.appendLog("anon CURRENT client conn count: " + (newCount - 1));
            logging.appendLog("anon MAX conn count: " + anonMaxCon);
            if (newCount > anonMaxCon) {
                Log.i(TAG, "Failed authentication, too many anonymous users connected.");
                Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
                sessionThread.writeString("421 too many anonymous users connected.\r\n");
                sessionThread.authAttempt(false);
            } else {
                Log.i(TAG, "Guest logged in with email: " + attemptPassword);
                sessionThread.writeString("230 Guest login ok, read only access.\r\n");
                final String anonChroot = sp.getString("anonChroot", "/storage/emulated/0" /*backwards compat*/);
                final String anonUriString = sp.getString("anonUriString", "");
                if (!anonChroot.isEmpty()) {
                    sessionThread.setChrootDir(anonChroot);
                    if (!anonUriString.isEmpty()) {
                        SessionThread.putUriString(Thread.currentThread().getName(), anonUriString);
                    } else if (Util.useScopedStorage()) {
                        // Protect against app crashes/problems
                        Log.i(TAG, "Failed authentication, too many anonymous users connected.");
                        Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
                        sessionThread.writeString("421 too many anonymous users connected.\r\n");
                        sessionThread.authAttempt(false);
                    }
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
            Log.i(TAG, "User " + user.getUsername() + " password verified");
            sessionThread.writeString("230 Access granted\r\n");
            sessionThread.authAttempt(true);
            sessionThread.setChrootDir(user.getChroot());
            if (Util.useScopedStorage()) {
                SessionThread.putUriString(Thread.currentThread().getName(), user.getUriString());
            }
        } else {
            Log.i(TAG, "Failed authentication, incorrect password");
            Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
            sessionThread.writeString("530 Login incorrect.\r\n");
            sessionThread.authAttempt(false);
            IPSecurity.putIPFail(sessionThread.getRemoteAddress());
        }
    }
}
