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

public class CmdPBSZ extends FtpCmd implements Runnable {
    private static final String TAG = CmdPBSZ.class.getSimpleName();
    protected String input;

    public CmdPBSZ(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "PBSZ executing");
        String param = getParameter(input);

        if (!param.equals("0")) sessionThread.writeString("500 Invalid command\r\n");

        sessionThread.setPbszEnabled(true);
        sessionThread.writeString("200 PBSZ command OK\r\n");

        Log.d(TAG, "PBSZ success");
    }
}