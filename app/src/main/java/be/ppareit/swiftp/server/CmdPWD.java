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

import android.net.Uri;
import android.util.Log;

import androidx.documentfile.provider.DocumentFile;

import java.io.File;
import java.io.IOException;

import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;

/**
 * PRINT WORKING DIRECTORY (PWD)
 * Command returns the working directory in the reply.
 */
public class CmdPWD extends FtpCmd implements Runnable {
    private static final String TAG = CmdPWD.class.getSimpleName();

    public CmdPWD(SessionThread sessionThread, String input) {
        super(sessionThread);
    }

    @Override
    public void run() {
        Log.d(TAG, "PWD executing");
        // The chroot restriction has been applied when the working directory was set, so
        // the user-visible path is the current directory with the chroot part taken off
        // the front. It cannot simply be sliced off by length: a chroot of "/" would lose
        // the leading slash, and getChrootDir() falls back to another directory entirely
        // once the session's own chroot stops existing, which can leave the working
        // directory outside it.
        try {
            String currentDir = sessionThread.getWorkingDir().getCanonicalPath();
            File chrootDir = sessionThread.getChrootDir();
            String visibleDir = "/";
            if (chrootDir != null) {
                visibleDir = chrootRelativePath(chrootDir.getCanonicalPath(), currentDir);
                if (visibleDir == null) {
                    Log.i(TAG, "Working dir lies outside the chroot, reporting the root");
                    visibleDir = "/";
                }
            }
            sessionThread.writeString("257 \"" + visibleDir + "\"\r\n");
        } catch (IOException e) {
            // This shouldn't happen unless our input validation has failed
            Log.e(TAG, "PWD canonicalize");
            sessionThread.closeSocket(); // should cause thread termination
        }
        Log.d(TAG, "PWD complete");
    }

}
