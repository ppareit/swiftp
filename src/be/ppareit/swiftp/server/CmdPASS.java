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

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;
import be.ppareit.swiftp.FtpServerApp;

public class CmdPASS extends FtpCmd implements Runnable {
    private static final String TAG = CmdPASS.class.getSimpleName();

    String input;

    public CmdPASS(SessionThread sessionThread, String input) {
        // We can just discard the password for now. We're just
        // following the expected dialogue, we're going to allow
        // access in any case.
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "Executing PASS");
        // User must have already executed a USER command to
        // populate the Account object's username
        String attemptPassword = getParameter(input, true); // silent
        String attemptUsername = sessionThread.account.getUsername();
        if (attemptUsername == null) {
            sessionThread.writeString("503 Must send USER first\r\n");
            return;
        }
        Context ctx = FtpServerApp.getAppContext();
        if (ctx == null) {
            // This will probably never happen, since the global
            // context is configured by the Service
            Log.e(TAG, "No global context in PASS\r\n");
        }
        String password;
        String username;
        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(ctx);
        username = settings.getString("username", null);
        password = settings.getString("password", null);
        if (username == null || password == null) {
            Log.e(TAG, "Username or password misconfigured");
            sessionThread.writeString("500 Internal error during authentication");
        } else if (username.equals(attemptUsername) && password.equals(attemptPassword)) {
            sessionThread.writeString("230 Access granted\r\n");
            Log.i(TAG, "User " + username + " password verified");
            sessionThread.authAttempt(true);
        } else {
            try {
                // If the login failed, sleep for one second to foil
                // brute force attacks
                Thread.sleep(1000);
            } catch (InterruptedException e) {
            }
            Log.i(TAG, "Failed authentication");
            sessionThread.writeString("530 Login incorrect.\r\n");
            sessionThread.authAttempt(false);
        }
    }
}
