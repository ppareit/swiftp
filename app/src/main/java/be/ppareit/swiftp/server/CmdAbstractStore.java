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

import androidx.documentfile.provider.DocumentFile;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;
import be.ppareit.swiftp.MediaUpdater;

abstract public class CmdAbstractStore extends FtpCmd {

    public CmdAbstractStore(SessionThread sessionThread, String input) {
        super(sessionThread);
    }

    public void doStorOrAppe(String param, boolean append) {
        Cat.d("STOR/APPE executing with append = " + append);

        File storeFile = inputPathToChrootedFile(sessionThread.getChrootDir(),
                sessionThread.getWorkingDir(), param, false);
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
            if (storeFile.isDirectory()) {
                errString = "451 Can't overwrite a directory\r\n";
                break storing;
            }
            if ((sessionThread.offset >= 0) && (append)) {
                errString = "555 Append can not be used after a REST command\r\n";
                break storing;
            }

            try {
                if (storeFile.exists()) {
                    if (!append && sessionThread.offset < 0) {
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

                if (!storeFile.exists()) {
                    if (Util.useScopedStorage()) {
                        final String mime = "application/octet-stream";
                        //final URI fileUri = storeFile.toURI();
                        //final URL url = fileUri.toURL();
                        //mime = url.openConnection().getContentType(); // sometimes makes exe's into html files
                        docStoreFile = FileUtil.mkfile(storeFile, App.getAppContext(), mime);
                        if(docStoreFile == null){
                            errString = "451 Couldn't open file \"" + param + "\" aka \""
                                    + storeFile.getCanonicalPath() + "\" for writing\r\n";
                            break storing;
                        }
                    } else {
                        FileUtil.mkfile(storeFile, App.getAppContext());
                    }
                }

                if (docStoreFile != null) {
                    os = App.getAppContext().getContentResolver().openOutputStream(docStoreFile.getUri());
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
                        if (sessionThread.offset < 0) {
                            out.getChannel().position(storeFile.length());
                        } else {
                            out.getChannel().position(sessionThread.offset);
                            sessionThread.offset = -1;
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
                            + storeFile.getCanonicalPath() + "\" for writing\r\n";
                } catch (IOException io_e) {
                    errString = "451 Couldn't open file, nested exception\r\n";
                }
                break storing;
            } catch (IOException e) {
                errString = "451 Unable to seek in file to append\r\n";
                break storing;
            }
            if (!sessionThread.openDataSocket()) {
                errString = "425 Couldn't open data socket\r\n";
                break storing;
            }
            Cat.d("Data socket ready");
            sessionThread.writeString("150 Data socket ready\r\n");
            byte[] buffer = new byte[SessionThread.DATA_CHUNK_SIZE];

            int numRead;

            Cat.d("Mode is " + (sessionThread.isBinaryMode() ? "binary" : "ascii"));

            while (true) {
                switch (numRead = sessionThread.receiveFromDataSocket(buffer)) {
                    case -1:
                        Cat.d("Returned from final read");
                        // We're finished reading
                        break storing;
                    case 0:
                        errString = "426 Couldn't receive data\r\n";
                        break storing;
                    case -2:
                        errString = "425 Could not connect data socket\r\n";
                        break storing;
                    default:
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
                        break;
                }
            }
        }
        try {
            if (out != null) {
                out.close();
            }
            if (os != null) {
                os.close();
            }
        } catch (IOException ignored) {
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
