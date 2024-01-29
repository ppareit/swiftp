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

import androidx.documentfile.provider.DocumentFile;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.MediaUpdater;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;

public class CmdRMD extends FtpCmd implements Runnable {
    private static final String TAG = CmdRMD.class.getSimpleName();

    protected String input;

    public CmdRMD(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "RMD executing");
        String param = getParameter(input);
        File toRemove;
        String errString = null;

        if (Util.useScopedStorage()) {
            final String path = sessionThread.getWorkingDir().getPath();
            final String finalParam = paramCheck(param, path);
            DocumentFile docFileToRemove = FileUtil.getDocumentFile(path + File.separator + finalParam);

            mainblock:
            {
                if (docFileToRemove == null){
                    errString = "550 Invalid name or chroot violation\r\n";
                    break mainblock;
                }
                if (param.length() < 1) {
                    errString = "550 Invalid argument\r\n";
                    break mainblock;
                }
                if (violatesChroot(docFileToRemove)) {
                    errString = "550 Invalid name or chroot violation\r\n";
                    break mainblock;
                }
                if (!docFileToRemove.isDirectory()) {
                    errString = "550 Can't RMD a non-directory\r\n";
                    break mainblock;
                }
                if (!recursiveDelete(docFileToRemove)) {
                    errString = "550 Deletion error, possibly incomplete\r\n";
                }
            }
            if (errString != null) {
                sessionThread.writeString(errString);
                Log.i(TAG, "RMD failed: " + errString.trim());
            } else {
                sessionThread.writeString("250 Removed directory\r\n");
            }
            Log.d(TAG, "RMD finished");
            return;
        }

        mainblock:
        {
            if (param.length() < 1) {
                errString = "550 Invalid argument\r\n";
                break mainblock;
            }
            toRemove = inputPathToChrootedFile(sessionThread.getChrootDir(),
                    sessionThread.getWorkingDir(), param);
            if (violatesChroot(toRemove)) {
                errString = "550 Invalid name or chroot violation\r\n";
                break mainblock;
            }
            if (!toRemove.isDirectory()) {
                errString = "550 Can't RMD a non-directory\r\n";
                break mainblock;
            }
            if (toRemove.equals(new File("/"))) {
                errString = "550 Won't RMD the root directory\r\n";
                break mainblock;
            }
            if (!recursiveDelete(toRemove)) {
                errString = "550 Deletion error, possibly incomplete\r\n";
                break mainblock;
            }
        }
        if (errString != null) {
            sessionThread.writeString(errString);
            Log.i(TAG, "RMD failed: " + errString.trim());
        } else {
            sessionThread.writeString("250 Removed directory\r\n");
        }
        Log.d(TAG, "RMD finished");
    }

    /*
    * In certain occasions (such as WinSCP dir delete) while under a sub path in the chroot, the
    * param here contains a duplication of that sub path and causes an issue. Other times (such as with
    * FileZilla), this doesn't happen. Seen with multiple clients so it is not being regarded as a bug.
    *
    * Discarding shows no ill effects nor conflicts. All paths seen as correct after this check.
    * CWD /a/a/a/b
    * 250 CWD successful
    * CWD /a/a/a
    * 250 CWD successful    (client knows)
    * RMD /a/a/a/b          (yet param is "/a/a/a/b" while getWorkingDir() is ".../a/a/a")
    * Only good part of the param is the last section "b"
    * Param is coming from the client directly with no changes.
    * If this continued without being changed then the path would wrongly become "/a/a/a/a/a/a/b"
    * */
    private String paramCheck(String param, String path) {
        String s = param;
        if (s.startsWith(File.separator)) s = s.substring(1);
        if (s.endsWith(File.separator)) s = s.substring(s.length() - 1);
        if (s.contains(File.separator)) {
            String s1 = s.substring(0, s.lastIndexOf(File.separator));
            if (path.contains(s1)) return s.substring(s.lastIndexOf(File.separator) + 1);
        }
        return s;
    }

    /**
     * Accepts a file or directory name, and recursively deletes the contents of that
     * directory and all subdirectories.
     *
     * @param toDelete
     * @return Whether the operation completed successfully
     */
    protected boolean recursiveDelete(DocumentFile toDelete) {
        if (!toDelete.exists()) {
            return false;
        }
        if (toDelete.isDirectory()) {
            // If any of the recursive operations fail, then we return false
            boolean success = true;
            for (DocumentFile entry : toDelete.listFiles()) {
                success &= recursiveDelete(entry);
            }
            Log.d(TAG, "Recursively deleted: " + toDelete);
            return success && toDelete.delete();
        } else {
            Log.d(TAG, "RMD deleting file: " + toDelete);
            return toDelete.delete();
        }
    }

    /**
     * Accepts a file or directory name, and recursively deletes the contents of that
     * directory and all subdirectories.
     *
     * @param toDelete
     * @return Whether the operation completed successfully
     */
    protected boolean recursiveDelete(File toDelete) {
        if (!toDelete.exists()) {
            return false;
        }
        if (toDelete.isDirectory()) {
            // If any of the recursive operations fail, then we return false
            boolean success = true;
            for (File entry : toDelete.listFiles()) {
                success &= recursiveDelete(entry);
            }
            Log.d(TAG, "Recursively deleted: " + toDelete);
            return success && FileUtil.deleteFile(toDelete, App.getAppContext());
        } else {
            Log.d(TAG, "RMD deleting file: " + toDelete);
            boolean success = FileUtil.deleteFile(toDelete, App.getAppContext());
            if (!Util.useScopedStorage()) {
                // Don't allow on Android 11+ as it causes problems should it reach here
                MediaUpdater.notifyFileDeleted(toDelete.getPath());
            }
            return success;
        }
    }
}
