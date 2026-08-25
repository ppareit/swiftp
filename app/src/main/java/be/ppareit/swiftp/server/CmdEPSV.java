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

import java.net.Inet6Address;
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

        if (param.equalsIgnoreCase("ALL")) {
            // RFC 2428 s4. only this, is what takes PASV, PORT and EPRT away for the session
            sessionThread.setEpsvAllRequested(true);
            sessionThread.writeString("200 EPSV ALL OK\r\n");
            Log.d(TAG, "EPSV ALL accepted");
            return;
        }

        // RFC 2428 s3: EPSV takes an optional network protocol, 1 for IPv4 and 2 for
        // IPv6. The data socket goes on the same address as the command socket, so
        // that address is the one protocol this session can offer. Asking for the one
        // we serve is a plain EPSV; asking for the other gets the 522 that names what
        // the client can use instead of leaving it to guess.
        boolean sessionIsIPv6 = sessionThread.getDataSocketPasvIp() instanceof Inet6Address;
        if (param.equals("1")) {
            if (sessionIsIPv6) {
                Log.d(TAG, "EPSV asked for IPv4 on an IPv6 session");
                sessionThread.writeString("522 Network protocol not supported, use (2)\r\n");
                return;
            }
        } else if (param.equals("2")) {
            if (!sessionIsIPv6) {
                Log.d(TAG, "EPSV asked for IPv6 on an IPv4 session");
                sessionThread.writeString("522 Network protocol not supported, use (1)\r\n");
                return;
            }
        } else if (!param.isEmpty()) {
            // 501, not 500: the command is recognised, its argument is not.
            Log.d(TAG, "EPSV invalid argument: " + param);
            sessionThread.writeString("501 Invalid EPSV argument\r\n");
            return;
        } else {
            Log.d(TAG, "Carry on as a plain EPSV");
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
        Log.d(TAG, "EPSV successful.");
    }
}