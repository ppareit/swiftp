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

/**
 * Since STOR and APPE are essentially identical except for append vs truncate,
 * the common code is in this class, and inherited by CmdSTOR and CmdAPPE.
 */

import android.os.ParcelFileDescriptor;

import androidx.documentfile.provider.DocumentFile;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.AllowedFolders;
import be.ppareit.swiftp.utils.FileUtil;
import be.ppareit.swiftp.utils.SafAccess;
import be.ppareit.swiftp.MediaUpdater;

abstract public class CmdAbstractStore extends FtpCmd {

    public CmdAbstractStore(SessionThread sessionThread, String input) {
        super(sessionThread);
    }

    public void doStorOrAppe(String param, boolean append) {
        long offset = sessionThread.offset;
        long endPosition = sessionThread.endPosition;
        sessionThread.clearRestartPosition();
        try (SafAccess.Lease ignored = Util.useScopedStorage() ? SafAccess.upload() : null) {
            store(param, append, offset, endPosition);
        }
    }

    private void store(String param, boolean append, long offset, long endPosition) {
        Cat.d("STOR/APPE executing with append = " + append);

        File storeFile = inputPathToChrootedFile(sessionThread.getChrootDir(),
                sessionThread.getWorkingDir(), param);
        DocumentFile docStoreFile = null;
        String errString = null;
        FileOutputStream out = null;
        OutputStream os = null;
        storing:
        {
            // Get a normalized absolute path for the desired file
            if (violatesChroot(storeFile)) {
                errString = "550 Invalid name or chroot violation\r\n";
                break storing;
            }
            if (Util.useScopedStorage() && AllowedFolders.isVirtual(storeFile.getParent())) {
                // We can't store in a virtual folder, tell the user
                errString = "553 Can't store here, store inside one of the allowed folders\r\n";
                break storing;
            }
            if (storeFile.isDirectory()) {
                errString = "451 Can't overwrite a directory\r\n";
                break storing;
            }
            if ((offset >= 0) && (append)) {
                errString = "555 Append can not be used after a REST command\r\n";
                break storing;
            }

            if (offset >= 0 && !sessionThread.isBinaryMode()) {
                errString = "550 Restart position not accepted in ASCII mode\r\n";
                break storing;
            }
            // Validate before creating or opening anything: a rejected resume must not
            // truncate an existing document or create an empty replacement.
            if (Util.useScopedStorage()) {
                docStoreFile = AllowedFolders.documentAt(storeFile.getPath());
                if (docStoreFile != null && docStoreFile.isDirectory()) {
                    errString = "451 Can't overwrite a directory\r\n";
                    break storing;
                }
            }
            long length = Util.useScopedStorage()
                    ? (docStoreFile != null && docStoreFile.exists() ? docStoreFile.length() : 0)
                    : storeFile.length();
            if (offset > length) {
                errString = "554 Restart position exceeds file length\r\n";
                break storing;
            }

            long remaining = offset >= 0 && endPosition >= offset
                    ? endPosition - offset : -1;
            // Store the inclusive range count without overflowing for 0..Long.MAX_VALUE.
            boolean bounded = remaining >= 0;
            if (bounded && remaining == Long.MAX_VALUE) {
                errString = "554 Requested range is too large\r\n";
                break storing;
            }
            if (bounded) remaining++;

            try {
                if (!Util.useScopedStorage() && storeFile.exists()) {
                    if (!append && offset < 0) {
                        if (!FileUtil.deleteFile(storeFile, App.getAppContext())) {
                            errString = "451 Couldn't truncate file\r\n";
                            break storing;
                        }
                        // Notify other apps that we just deleted a file
                        if (!Util.useScopedStorage()) {
                            // don't allow on Android 11+ as it causes problems
                            MediaUpdater.notifyFileDeleted(storeFile.getPath());
                        }
                    }
                }

                if (Util.useScopedStorage()) {
                    final String mime = "application/octet-stream";
                    docStoreFile = FileUtil.mkfile(storeFile, mime);
                    if (docStoreFile == null || !docStoreFile.exists()) {
                        errString = "451 Couldn't open file \"" + param + "\" aka \""
                                + storeFile.getCanonicalPath() + "\" for writing\r\n";
                        break storing;
                    }
                } else {
                    FileUtil.mkfile(storeFile, App.getAppContext());
                }

                if (docStoreFile != null) {
                    if (offset >= 0) {
                        // "rw" preserves the document. Seek before accepting data; a
                        // provider without seek support must fail rather than corrupt it.
                        ParcelFileDescriptor descriptor = App.getAppContext().getContentResolver()
                                .openFileDescriptor(docStoreFile.getUri(), "rw");
                        if (descriptor == null) throw new IOException("No file descriptor");
                        out = new ParcelFileDescriptor.AutoCloseOutputStream(descriptor);
                    } else {
                        os = App.getAppContext().getContentResolver().openOutputStream(
                                docStoreFile.getUri(), append ? "wa" : "wt");
                        if (os == null) throw new IOException("No output stream");
                    }
                } else {
                    out = FileUtil.getOutputStream(storeFile, App.getAppContext());
                }

                //file
                if(docStoreFile == null && out == null){
                    errString = "451 Couldn't open file \"" + param + "\" aka \""
                            + storeFile.getCanonicalPath() + "\" for writing\r\n";
                    break storing;
                }
                try {
                    if (out != null) {
                        if (offset < 0) {
                            out.getChannel().position(storeFile.length());
                        } else {
                            out.getChannel().position(offset);
                        }
                    }
                }
                catch (NullPointerException e){
                    errString = "451 Couldn't open file \"" + param + "\" aka \""
                            + storeFile.getCanonicalPath() + "\" for writing. Failed to create output stream.\r\n";
                    break storing;
                }

            } catch (FileNotFoundException e) {
                Cat.e("error : ", e);
                try {
                    errString = "451 Couldn't open file \"" + param + "\" aka \""
                            + storeFile.getCanonicalPath()
                            + "\" for writing. Check storage access and mount status.\r\n";
                } catch (IOException io_e) {
                    errString = "451 Couldn't open file, nested exception\r\n";
                }
                break storing;
            } catch (IOException e) {
                errString = "451 Unable to seek in file to append\r\n";
                break storing;
            }
            // Before openDataSocket, not after: see the note in CmdRETR. A 425 after a
            // 150 is the sequence RFC 959 already describes for a data socket that never
            // came up, so nothing is lost by promising the transfer first.
            sessionThread.writeString("150 Data socket ready\r\n");
            if (!sessionThread.openDataSocket()) {
                errString = "425 " + sessionThread.getDataSocketErrorMessage() + "\r\n";
                break storing;
            }
            Cat.d("Data socket ready");
            byte[] buffer = new byte[SessionThread.DATA_CHUNK_SIZE];

            int numRead;

            Cat.d("Mode is " + (sessionThread.isBinaryMode() ? "binary" : "ascii"));

            while (true) {
                switch (numRead = sessionThread.receiveFromDataSocket(buffer)) {
                    case -1:
                        Cat.d("Returned from final read");
                        if (bounded && remaining != 0) {
                            errString = "426 Incomplete range upload\r\n";
                        }
                        break storing;
                    case 0:
                        errString = "426 Couldn't receive data\r\n";
                        break storing;
                    case -2:
                        errString = "425 Could not connect data socket\r\n";
                        break storing;
                    default:
                        boolean excess = bounded && numRead > remaining;
                        if (excess) numRead = (int) remaining;
                        try {
                            if (sessionThread.isBinaryMode()) {
                                if (os != null) os.write(buffer, 0, numRead);
                                else out.write(buffer, 0, numRead);
                            } else {
                                // ASCII mode, substitute \r\n to \n
                                int startPos = 0, endPos;
                                for (endPos = 0; endPos < numRead; endPos++) {
                                    if (buffer[endPos] == '\r') {
                                        if (os != null) os.write(buffer, startPos, endPos - startPos);
                                        else out.write(buffer, startPos, endPos - startPos);
                                        // Our hacky method is to drop all \r
                                        startPos = endPos + 1;
                                    }
                                }
                                // Write last part of buffer as long as there was something
                                // left after handling the last \r
                                if (startPos < numRead) {
                                    if (os != null) os.write(buffer, startPos, endPos - startPos);
                                    else if (out != null) out.write(buffer, startPos, endPos - startPos);
                                }
                            }
                        } catch (IOException e) {
                            errString = "451 File IO problem. Device might be full.\r\n";
                            Cat.d("Exception while storing: " + e);
                            break storing;
                        }
                        if (bounded) remaining -= numRead;
                        if (excess) {
                            errString = "426 Upload exceeds requested range\r\n";
                            break storing;
                        }
                        break;
                }
            }
        }
        try {
            if (os != null) {
                os.close();
            }
        } catch (IOException e) {
            if (errString == null) errString = "451 Could not finish writing file\r\n";
        }
        try {
            if (out != null) {
                out.close();
            }
        } catch (IOException e) {
            if (errString == null) errString = "451 Could not finish writing file\r\n";
        }

        if (errString != null) {
            Cat.i("STOR error: " + errString.trim());
            sessionThread.writeString(errString);
        } else {
            sessionThread.writeString("226 Transmission complete\r\n");
            // Notify the music player (and possibly others) that a few file has
            // been uploaded.
            if (!Util.useScopedStorage()) {
                // don't allow on Android 11+ as it causes problems
                MediaUpdater.notifyFileCreated(storeFile.getPath());
            }
        }
        sessionThread.closeDataSocket();
        Cat.d("STOR finished");
    }
}
