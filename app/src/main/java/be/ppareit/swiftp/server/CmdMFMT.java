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
import java.util.TimeZone;

import androidx.documentfile.provider.DocumentFile;

import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;

import net.vrallev.android.cat.Cat;

/**
 * CmdMFMT implements File Modification Time. See draft-somers-ftp-mfxx-04 in documentation.
 */
public class CmdMFMT extends FtpCmd implements Runnable {

    private String mInput;

    public CmdMFMT(SessionThread sessionThread, String input) {
        super(sessionThread);
        mInput = input;
    }

    @Override
    public void run() {
        Cat.d("run: MFMT executing, input: " + mInput);

        //Syntax: "MFMT" SP time-val SP pathname CRLF
        String parameter = getParameter(mInput);
        int splitPosition = parameter.indexOf(' ');
        if (splitPosition == -1) {
            sessionThread.writeString("500 wrong number of parameters\r\n");
            Cat.d("run: MFMT failed, wrong number of parameters");
            return;
        }

        String timeString = parameter.substring(0, splitPosition);
        String pathName = parameter.substring(splitPosition + 1);

        // Format of time-val: YYYYMMDDHHMMSS.ss, see rfc3659, p6
        // BUG: The milliseconds part get's ignored
        SimpleDateFormat df = new SimpleDateFormat("yyyyMMddhhmmss", Locale.US);
        df.setTimeZone(TimeZone.getTimeZone("UTC"));

        Date timeVal;
        try {
            timeVal = Util.parseDate(timeString);
        } catch (ParseException e) {
            sessionThread.writeString("501 unable to parse parameter time-val\r\n");
            Cat.d("run: MFMT failed, unable to parse parameter time-val");
            return;
        }

        File file = inputPathToChrootedFile(sessionThread.getChrootDir(),
                sessionThread.getWorkingDir(), pathName);

        if (violatesChroot(file)) {
            sessionThread.writeString("550 Invalid name or chroot violation\r\n");
            Cat.d("run: MFMT failed, chroot violation");
            return;
        }

        // SAF exposes modification time as read-only metadata. When direct access is also
        // available, use the real path to add timestamp support for the selected folder.
        FileUtil.Gen target = FileUtil.createGenFromFile(file);
        if (Util.hasFullSdCardAccess() && target.getOb() instanceof DocumentFile) {
            target = FileUtil.convertFileToGen(file);
        }

        if (!target.exists()) {
            sessionThread.writeString("550 file does not exist on server\r\n");
            Cat.d("run: MFMT failed, file does not exist");
            return;
        }

        if (!(target.getOb() instanceof File targetFile)) {
            sessionThread.writeString("500 unable to modify last modification time\r\n");
            Cat.d("run: MFMT failed, direct file access is unavailable");
            return;
        }

        if (!targetFile.setLastModified(timeVal.getTime())) {
            sessionThread.writeString("500 unable to modify last modification time\r\n");
            Cat.d("run: MFMT failed, unable to modify last modification time");
            // more info at https://code.google.com/p/android/issues/detail?id=18624
            return;
        }

        long lastModified = target.lastModified();
        String response = "213 " + df.format(new Date(lastModified)) + "; "
                + file.getAbsolutePath() + "\r\n";
        sessionThread.writeString(response);

        Cat.d("run: MFMT completed successful");
    }

}
