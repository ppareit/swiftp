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

public class CmdPROT extends FtpCmd implements Runnable {
    private static final String TAG = CmdPROT.class.getSimpleName();
    protected String input;

    public CmdPROT(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "PROT executing");
        String param = getParameter(input);

        if (!sessionThread.isPbszEnabled()) {
            sessionThread.writeString("500 Failure: PBSZ command was not issued\r\n");
            return;
        }

        switch (param) {
            case "P" -> sessionThread.writeString("200 PROT command OK\r\n");
            case "C" -> sessionThread.writeString("536 Protection level not supported\r\n");
            case "S" -> sessionThread.writeString("536 Protection level not supported\r\n");
            case "E" -> sessionThread.writeString("536 Protection level not supported\r\n");
            default -> sessionThread.writeString("502 Command not recognized\r\n");
        }

        Log.d(TAG, "PROT success");
    }
}