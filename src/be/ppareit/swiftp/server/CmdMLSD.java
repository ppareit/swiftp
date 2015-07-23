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

/* The code that is common to LIST and NLST is implemented in the abstract
 * class CmdAbstractListing, which is inherited here.
 * CmdLIST and CmdNLST just override the
 * makeLsString() function in different ways to provide the different forms
 * of output.
 */

package be.ppareit.swiftp.server;

import java.io.File;

import android.util.Log;
import be.ppareit.swiftp.Util;

public class CmdMLSD extends CmdAbstractListing implements Runnable {
    static private final String TAG = CmdMLSD.class.getSimpleName();

    private final String input;

    public CmdMLSD(SessionThread sessionThread, String input) {
        super(sessionThread, input);
        this.input = input;
    }

    @Override
    public void run() {
        String errString = null;

        mainblock: {
            String param = getParameter(input);
            Log.d(TAG, "MLSD parameter: " + param);
            while (param.startsWith("-")) {
                // Skip all dashed -args, if present
                Log.d(TAG, "MLSD is skipping dashed arg " + param);
                param = getParameter(param);
            }
            File fileToList = null;
            if (param.equals("")) {
                fileToList = sessionThread.getWorkingDir();
            } else {
                if (param.contains("*")) {
                    errString = "550 MLSD does not support wildcards\r\n";
                    break mainblock;
                }
                fileToList = new File(sessionThread.getWorkingDir(), param);
                if (violatesChroot(fileToList)) {
                    errString = "450 MLSD target violates chroot\r\n";
                    break mainblock;
                }
            }
            String listing;
            if (fileToList.isDirectory()) {
                StringBuilder response = new StringBuilder();
                errString = listDirectory(response, fileToList);
                if (errString != null) {
                    break mainblock;
                }
                listing = response.toString();
            } else {
                listing = makeLsString(fileToList);
                if (listing == null) {
                    errString = "501 Not a directory\r\n";
                    break mainblock;
                }
            }
            errString = sendListing(listing);
            if (errString != null) {
                break mainblock;
            }
        }

        if (errString != null) {
            sessionThread.writeString(errString);
            Log.d(TAG, "MLSD failed with: " + errString);
        } else {
            Log.d(TAG, "MLSD completed OK");
        }
        // The success or error response over the control connection will
        // have already been handled by sendListing, so we can just quit now.
    }

    // Generates a line of a directory listing in the traditional /bin/ls
    // format.
    @Override
    protected String makeLsString(File file) {      
        StringBuilder response = new StringBuilder();

        if (!file.exists()) {
            Log.i(TAG, "makeLsString had nonexistent file");
            return null;
        }

        // See Daniel Bernstein's explanation of /bin/ls format at:
        // http://cr.yp.to/ftp/list/binls.html
        // This stuff is almost entirely based on his recommendations.

        String lastNamePart = file.getName();
        // Many clients can't handle files containing these symbols
        if (lastNamePart.contains("*") || lastNamePart.contains("/")) {
            Log.i(TAG, "Filename omitted due to disallowed character");
            return null;
        } else {
            // The following line generates many calls in large directories
            // staticLog.l(Log.DEBUG, "Filename: " + lastNamePart);
        }

        String[] selectedTypes = sessionThread.getFormatTypes();   
        if(selectedTypes != null){
            for (int i = 0; i < selectedTypes.length; ++i) {
                String type = selectedTypes[i];
                if (type.equalsIgnoreCase("size")) {
                    response.append("Size=" + String.valueOf(file.length()) + ';');
                } else if (type.equalsIgnoreCase("modify")) {
                    String timeStr = Util.getFtpDate(file.lastModified());
                    response.append("Modify=" + timeStr + ';');
                } else if (type.equalsIgnoreCase("type")) {
                    if (file.isFile()) {
                        response.append("Type=file;");
                    } else if (file.isDirectory()) {
                        response.append("Type=dir;");
                    }
                } else if (type.equalsIgnoreCase("perm")) {
                    response.append("Perm=");
                    if (file.canRead()) {
                        if (file.isFile()) {
                            response.append('r');
                        } else if (file.isDirectory()) {
                            response.append("el");
                        }
                    }
                    if (file.canWrite()) {
                        if (file.isFile()) {
                            response.append("adfw");
                        } else if (file.isDirectory()) {
                            response.append("fpcm");
                        }
                    }
                    response.append(';');
                }
            }
        }

        response.append(' ');    
        response.append(lastNamePart);
        response.append("\r\n");
        return response.toString();
    }

}
