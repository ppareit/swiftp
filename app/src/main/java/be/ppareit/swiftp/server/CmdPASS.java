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
        FtpUser user = FsSettings.getUser(attemptUsername);
        if (user == null) {
            Log.e(TAG, "Username does not exist!");
            sessionThread.writeString("500 Internal error during authentication");
        } else if (user.getPassword().equals(attemptPassword)) {
            sessionThread.writeString("230 Access granted\r\n");
            Log.i(TAG, "User " + user.getUsername() + " password verified");
            sessionThread.authAttempt(true);
            sessionThread.setChrootDir(user.getChroot());
        } else if (attemptUsername.equals("anonymous") && FsSettings.allowAnoymous()) {
            sessionThread.writeString("230 Guest login ok, read only access.\r\n");
            Log.i(TAG, "Guest logged in with email: " + attemptPassword);
        } else {
            Log.i(TAG, "Failed authentication");
            Util.sleepIgnoreInterrupt(1000); // sleep to foil brute force attack
            sessionThread.writeString("530 Login incorrect.\r\n");
            sessionThread.authAttempt(false);
        }
    }
}
