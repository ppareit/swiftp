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

import java.io.File;

import android.util.Log;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;
import be.ppareit.swiftp.MediaUpdater;
import be.ppareit.swiftp.utils.SwiftpFile;

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
        SwiftpFile file = new SwiftpFile(inputPathToChrootedFile(sessionThread.getChrootDir(),
                sessionThread.getWorkingDir(), param));

        if (Util.useScopedStorage()) {
            String clientPath;
            final String sfPath = file.getPath();
            if (sfPath.contains(File.separator)) {
                clientPath = sfPath.substring(0, sfPath.lastIndexOf(File.separator));
            } else {
                clientPath = sfPath;
            }
            file = new SwiftpFile(FileUtil.getDocumentFileWithParamScopedStorage(File.separator + param, null, clientPath));
        }

        tryToDelete(file, param);
    }

    private void tryToDelete(SwiftpFile file, String param) {
        String errString = null;
        if (file == null) {
            errString = "550 Invalid name or chroot violation\r\n";
        } else {
            final String path = FileUtil.getScopedClientPath(param, null, null);
            if (file.violatesChroot(this, path)) {
                errString = "550 Invalid name or chroot violation\r\n";
            } else if (file.isDirectory()) {
                errString = "550 Can't DELE a directory\r\n";
            } else if (!file.delete(App.getAppContext())) {
                errString = "450 Error deleting file\r\n";
            }
        }

        if (errString != null) {
            sessionThread.writeString(errString);
            Log.i(TAG, "DELE failed: " + errString.trim());
        } else {
            sessionThread.writeString("250 File successfully deleted\r\n");
            if (!Util.useScopedStorage()) {
                // don't allow on Android 11+ as it causes problems
                MediaUpdater.notifyFileDeleted(file.getPath());
            }
        }
        Log.d(TAG, "DELE finished");
    }

}
