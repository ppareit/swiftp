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

import android.util.Log;
import be.ppareit.swiftp.Util;

/**
 * Implements MLST command
 */
public class CmdMLST extends FtpCmd implements Runnable {
    private static final String TAG = CmdMLST.class.getSimpleName();

    private String mInput;

    public CmdMLST(SessionThread sessionThread, String input) {
        super(sessionThread);
        mInput = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "run: LIST executing, input: " + mInput);
        String param = getParameter(mInput);
        
        File fileToFormat = null;
        if(param.equals("")){
            fileToFormat = sessionThread.getWorkingDir();
            param = "/";
        }else{
            fileToFormat = inputPathToChrootedFile(sessionThread.getChrootDir(), sessionThread.getWorkingDir(), param);
        }
        
        if (fileToFormat.exists()) {
            sessionThread.writeString("250- Listing " + param + "\r\n");
            sessionThread.writeString(makeString(fileToFormat) + "\r\n");
            sessionThread.writeString("250 End\r\n");
        } else {
            Log.w(TAG, "run: file does not exist");
            sessionThread.writeString("550 file does not exist\r\n");
        }

        Log.d(TAG, "run: LIST completed");
    }

    public String makeString(File file){
        StringBuilder response = new StringBuilder();
        response.append(sessionThread.makeSelectedTypesResponse(file));
        response.append(' ');
        response.append(file.getName());
        response.append("\r\n");
        return response.toString();
    }
}

