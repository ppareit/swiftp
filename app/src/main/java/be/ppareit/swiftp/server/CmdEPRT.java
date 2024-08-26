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
import java.net.NetworkInterface;

public class CmdEPRT extends FtpCmd implements Runnable {
    private static final String TAG = CmdEPRT.class.getSimpleName();
    protected String input;

    public CmdEPRT(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "EPRT executing");
        String param = getParameter(input);

        if (param.isEmpty()) {
            Log.e(TAG, "Bad EPRT command");
            sessionThread.writeString("500 Empty command is invalid\r\n");
            return;
        }

        // TODO implement IPv4
        if (param.contains("|1|")) {
            // IPv4 isn't implemented fpr EPRT
            sessionThread.writeString("502 IPv4 command not implemented.\r\n");
            return;
        }

        // From client eg EPRT |2|IPv6Address|Port|
        //  |1| is IPv4, |2| is IPv6
        //  IPv6Address is the address to use with the socket
        //  Port is the port to use with the socket
        String addressString = param.replace("|2|", "");
        String portString = addressString.substring(addressString.indexOf("|") + 1);
        portString = portString.replace("|", "");
        addressString = addressString.substring(0, addressString.indexOf("|"));
        addressString = addressString.replace(portString, "");

        try {
            String a1 = addressString.replaceFirst("/", "");
            byte[] addr = Inet6Address.getByName(a1).getAddress();
            NetworkInterface ni = NetworkInterface.getByInetAddress(sessionThread.getDataSocketPasvIp());
            // Scope ID required for IPv6 link local address
            Inet6Address inet6Address = Inet6Address.getByAddress(a1, addr, ni);
            sessionThread.onEprt(inet6Address, Integer.parseInt(portString));
        } catch (Exception e) {
            // There was a problem opening a port
            Log.e(TAG, "Failed to open port:" + portString + ", " + addressString + ", " + e.getMessage());
            sessionThread.writeString("500 Failed to open port\r\n");
            return;
        }

        sessionThread.writeString("200 EPRT OK\r\n");
        sessionThread.setEprtEnabled(true);
        Log.d(TAG, "EPRT success");
    }
}