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

import androidx.documentfile.provider.DocumentFile;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.FileUtil;

public class CmdRETR extends FtpCmd implements Runnable {

    protected String input;

    public CmdRETR(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Cat.d("RETR executing");
        String param = getParameter(input);
        File fileToRetr;
        String errString = null;

        mainblock:
        {
            fileToRetr = inputPathToChrootedFile(sessionThread.getChrootDir(),
                    sessionThread.getWorkingDir(), param);
            Uri uri;

            DocumentFile docFileToRetr = null;
            if (Util.useScopedStorage()) {
                String clientPath;
                final String ftrPath = fileToRetr.getPath();
                if (ftrPath.contains(File.separator)) {
                    clientPath = ftrPath.substring(0, ftrPath.lastIndexOf(File.separator));
                } else {
                    clientPath = ftrPath;
                }
                docFileToRetr = FileUtil.getDocumentFileWithParamScopedStorage(param, null, clientPath);
                if (docFileToRetr == null) {
                    errString = "550 File does not exist\r\n";
                    break mainblock;
                }
            }

            FileUtil.Gen gen;
            if (docFileToRetr != null) gen = FileUtil.convertDocumentFileToGen(docFileToRetr);
            else gen = FileUtil.convertFileToGen(fileToRetr);
            if (validate(gen, param) != null) break mainblock;

            FileInputStream in = null;
            InputStream is = null;
            try {
                if (Util.useScopedStorage()) {
                    if (docFileToRetr == null) break mainblock;
                    uri = docFileToRetr.getUri();
                    is = App.getAppContext().getContentResolver().openInputStream(uri);
                } else {
                    in = new FileInputStream(fileToRetr);
                }
                byte[] buffer = new byte[SessionThread.DATA_CHUNK_SIZE];
                int bytesRead;
                if (sessionThread.openDataSocket()) {
                    Cat.d("RETR opened data socket");
                } else {
                    errString = "425 Error opening socket\r\n";
                    Cat.i("Error in initDataSocket()");
                    break mainblock;
                }
                sessionThread.writeString("150 Sending file\r\n");
                if (sessionThread.isBinaryMode()) { // RANG is supported only in binary mode.
                    Cat.d("Transferring in binary mode");
                    long offset = 0L;
                    long endPosition = (Util.useScopedStorage() ? docFileToRetr.length() - 1
                            : fileToRetr.length() - 1);
                    if (sessionThread.offset >= 0) {
                        offset = sessionThread.offset;
                        if (sessionThread.endPosition >= offset) {
                            endPosition = sessionThread.endPosition;
                        }
                        sessionThread.offset = -1;
                    }
                    // This is not a range but length (Range 0-0 would still read 0th byte), so +1
                    long bytesToRead = endPosition - offset + 1;
                    if (Util.useScopedStorage()) is.skip(offset);
                    else in.skip(offset);
                    while ((bytesRead = (Util.useScopedStorage() ? is.read(buffer) : in.read(buffer))) != -1) {
                        boolean success;
                        if (bytesRead > bytesToRead) {
                            success = sessionThread.sendViaDataSocket(buffer, 0, (int) bytesToRead);
                        } else {
                            success = sessionThread.sendViaDataSocket(buffer, 0, bytesRead);
                            bytesToRead -= bytesRead;
                        }

                        if (!success) {
                            errString = "426 Data socket error\r\n";
                            Cat.i("Data socket error");
                            break mainblock;
                        }
                    }
                } else { // We're in ASCII mode
                    Cat.d("Transferring in ASCII mode");
                    if (sessionThread.offset >= 0) {
                        errString = "550 Unable to seek to requested position in ASCII mode";
                        Cat.e("Error: " + errString);
                        break mainblock;
                    }
                    // We have to convert all solitary \n to \r\n
                    boolean lastBufEndedWithCR = false;
                    while ((bytesRead = (Util.useScopedStorage() ? is.read(buffer) : in.read(buffer))) != -1) {
                        int startPos = 0, endPos = 0;
                        byte[] crnBuf = {'\r', '\n'};
                        for (endPos = 0; endPos < bytesRead; endPos++) {
                            if (buffer[endPos] == '\n') {
                                // Send bytes up to but not including the newline
                                sessionThread.sendViaDataSocket(buffer, startPos, endPos
                                        - startPos);
                                if (endPos == 0) {
                                    // handle special case where newline occurs at
                                    // the beginning of a buffer
                                    if (!lastBufEndedWithCR) {
                                        // Send an \r only if the the previous
                                        // buffer didn't end with an \r
                                        sessionThread.sendViaDataSocket(crnBuf, 0, 1);
                                    }
                                } else if (buffer[endPos - 1] != '\r') {
                                    // The file did not have \r before \n, add it
                                    sessionThread.sendViaDataSocket(crnBuf, 0, 1);
                                } else {
                                    // The file did have \r before \n, don't change
                                }
                                startPos = endPos;
                            }
                        }
                        // Now endPos has finished traversing the array, send remaining data as-is
                        sessionThread.sendViaDataSocket(buffer, startPos, endPos - startPos);
                        if (buffer[bytesRead - 1] == '\r') {
                            lastBufEndedWithCR = true;
                        } else {
                            lastBufEndedWithCR = false;
                        }
                    }
                }
            } catch (FileNotFoundException e) {
                errString = "550 File not found\r\n";
                break mainblock;
            } catch (IOException e) {
                errString = "425 Network error\r\n";
                break mainblock;
            } finally {
                try {
                    if (in != null)
                        in.close();
                    if (is != null)
                        is.close();
                } catch (IOException ignored) {
                }
            }
        }
        sessionThread.closeDataSocket();
        if (errString != null) {
            sessionThread.writeString(errString);
        } else {
            sessionThread.writeString("226 Transmission finished\r\n");
        }
        Cat.d("RETR done");
    }

    private String validate(FileUtil.Gen fileToRetr, String param) {
        String errString = null;
        if (fileToRetr == null) {
            errString = "550 Invalid name or chroot violation\r\n";
            return errString;
        }

        final boolean isDocumentFile = fileToRetr.getOb() instanceof DocumentFile;
        final boolean isFile = !isDocumentFile;

        String clientPath;
        String path = null;
        if (isDocumentFile) {
            clientPath = param;
            if (!param.contains(File.separator)) clientPath = "";
            else if (!param.endsWith(File.separator)) clientPath = param.substring(0, param
                    .lastIndexOf(File.separator));
            path = FileUtil.getScopedClientPath(clientPath, null, null);
        }
        if ((isDocumentFile && violatesChroot((DocumentFile) fileToRetr.getOb(), path))
                || (isFile && violatesChroot((File) fileToRetr.getOb()))) {
            errString = "550 Invalid name or chroot violation\r\n";
        } else if (fileToRetr.isDirectory()) {
            Cat.d("Ignoring RETR for directory");
            errString = "550 Can't RETR a directory\r\n";
        } else if (!fileToRetr.exists()) {
            if (isDocumentFile) Cat.d("Can't RETR nonexistent file: " + fileToRetr.getName());
            else Cat.d("Can't RETR nonexistent file: " + ((File)fileToRetr.getOb()).getAbsolutePath());
            errString = "550 File does not exist\r\n";
        } else if (!fileToRetr.canRead()) {
            Cat.i("Failed RETR permission (canRead() is false)");
            errString = "550 No read permissions\r\n";
        }
        return errString;
    }
}
