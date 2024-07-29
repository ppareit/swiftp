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

import java.net.InetAddress;

public class CmdEPSV extends FtpCmd implements Runnable {
    private static final String TAG = CmdEPSV.class.getSimpleName();
    protected String input;

    public CmdEPSV(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "EPSV executing");
        String param = getParameter(input);

        if (!param.isEmpty()) {
            sessionThread.writeString("500 Invalid command\r\n");
            Log.d(TAG, "EPSV invalid command: " + param);
            return;
        }

        int port;
        InetAddress address;
        address = sessionThread.getDataSocketPasvIp();
        // Socket for EPSV requires the address of the device that Swiftp is running on.
        if ((port = sessionThread.onEpsv(address)) == 0) {
            // There was a problem opening a port
            Log.e(TAG, "Failed to open port");
            sessionThread.writeString("500 Failed to open port\r\n");
            return;
        }

        final String responseString = "229 Entering Extended Passive Mode (|||" + port + "|)\r\n";
        sessionThread.writeString(responseString);
        sessionThread.setEpsvEnabled(true);
        Log.d(TAG, "EPSV successful.");
    }
}