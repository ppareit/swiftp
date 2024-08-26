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

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.utils.FTPSSockets;

public class CmdAUTH extends FtpCmd implements Runnable {
    private static final String TAG = CmdPROT.class.getSimpleName();
    protected String input;

    public CmdAUTH(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "AUTH executing");
        String param = getParameter(input);

        if (param.contains("TLS")) {
            sessionThread.writeString("234 AUTH command OK. Initializing TLS connection.\r\n");
            sessionThread.cmdSSLAuthSocket = new FTPSSockets().createAuthSocket("TLS", sessionThread.cmdSocket);
        } else if (param.contains("SSL")) {
            if (FsSettings.useSSL()) {
                sessionThread.writeString("234 AUTH command OK. Initializing SSL connection.\r\n");
                sessionThread.cmdSSLAuthSocket = new FTPSSockets().createAuthSocket("SSL", sessionThread.cmdSocket);
            }
        }

        if (sessionThread.cmdSSLAuthSocket == null) {
            // After this, the connection tested good with being dropped and socket closed
            sessionThread.writeString("431 AUTH failed\r\n");
            Log.d(TAG, "AUTH failed");
            return;
        }

        Log.d(TAG, "AUTH success");
    }
}