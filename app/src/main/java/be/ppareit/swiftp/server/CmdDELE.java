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

import java.io.File;

import be.ppareit.swiftp.MediaUpdater;

public class CmdDELE extends FtpCmd implements Runnable {
    private static final String TAG = CmdDELE.class.getSimpleName();

    protected String input;

    public CmdDELE(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "DELE executing");
        String param = getParameter(input);
        File storeFile = inputPathToChrootedFile(sessionThread.getWorkingDir(), param);
        String errString = null;
        if (violatesChroot(storeFile)) {
            errString = "550 Invalid name or chroot violation\r\n";
        } else if (storeFile.isDirectory()) {
            errString = "550 Can't DELE a directory\r\n";
        } else if (!storeFile.delete()) {
            errString = "450 Error deleting file\r\n";
        }

        if (errString != null) {
            sessionThread.writeString(errString);
            Log.i(TAG, "DELE failed: " + errString.trim());
        } else {
            sessionThread.writeString("250 File successfully deleted\r\n");
            MediaUpdater.notifyFileDeleted(storeFile.getPath());
        }
        Log.d(TAG, "DELE finished");
    }

}
