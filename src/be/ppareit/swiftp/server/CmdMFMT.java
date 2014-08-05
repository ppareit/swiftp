/*
Copyright 2014 Pieter Pareit

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

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import android.util.Log;

/**
 * Implements File Modification Time
 */
public class CmdMFMT extends FtpCmd implements Runnable {
    private static final String TAG = CmdMFMT.class.getSimpleName();

    private String mInput;

    public CmdMFMT(SessionThread sessionThread, String input) {
        super(sessionThread);
        mInput = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "run: MFMT executing, input: " + mInput);
        String[] params = getParameter(mInput).split(" ");

        if (params.length != 2) {
            sessionThread.writeString("500 wrong number of parameters\r\n");
            Log.d(TAG, "run: MFMT failed, wrong number of parameters");
            return;
        }

        // Format of time-val: YYYYMMDDHHMMSS.ss, see rfc3659, p6
        // BUG: The milliseconds part get's ignored
        SimpleDateFormat df = new SimpleDateFormat("yyyyMMddhhmmss", Locale.US);

        Date timeVal;
        try {
            timeVal = df.parse(params[0]);
        } catch (ParseException e) {
            sessionThread.writeString("501 unable to parse parameter time-val\r\n");
            Log.d(TAG, "run: MFMT failed, unable to parse parameter time-val");
            return;
        }

        String pathName = params[1];
        File file = inputPathToChrootedFile(sessionThread.getWorkingDir(), pathName);

        if (file.exists() == false) {
            sessionThread.writeString("550 file does not exist on server\r\n");
            Log.d(TAG, "run: MFMT failed, file does not exist");
            return;
        }

        boolean success = file.setLastModified(timeVal.getTime());
        if (success == false) {
            sessionThread.writeString("500 unable to modify last modification time\r\n");
            Log.d(TAG, "run: MFMT failed, unable to modify last modification time");
            // more info at
            // https://code.google.com/p/android/issues/detail?id=18624
            return;
        }

        long lastModified = file.lastModified();
        String response = "213 " + df.format(new Date(lastModified)) + "; "
                + file.getAbsolutePath() + "\r\n";
        sessionThread.writeString(response);

        Log.d(TAG, "run: MFMT completed succesful");
    }

}

