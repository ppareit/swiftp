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

public class CmdOPTS extends FtpCmd implements Runnable {
    private static final String TAG = CmdOPTS.class.getSimpleName();

    public static final String message = "TEMPLATE!!";
    private final String input;

    public CmdOPTS(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        String param = getParameter(input);
        String errString = null;

        mainBlock: {
            if (param == null) {
                errString = "550 Need argument to OPTS\r\n";
                Log.w(TAG, "Couldn't understand empty OPTS command");
                break mainBlock;
            }
            String[] splits = param.split(" ");
            if (splits.length != 2) {
                errString = "550 Malformed OPTS command\r\n";
                Log.w(TAG, "Couldn't parse OPTS command");
                break mainBlock;
            }
            String optName = splits[0].toUpperCase();
            String optVal = splits[1].toUpperCase();
            if (optName.equals("UTF8")) {
                // OK, whatever. Don't really know what to do here. We
                // always operate in UTF8 mode.
                if (optVal.equals("ON")) {
                    Log.d(TAG, "Got OPTS UTF8 ON");
                    sessionThread.setEncoding("UTF-8");
                } else {
                    Log.i(TAG, "Ignoring OPTS UTF8 for something besides ON");
                }
                break mainBlock;
            } else {
                Log.d(TAG, "Unrecognized OPTS option: " + optName);
                errString = "502 Unrecognized option\r\n";
                break mainBlock;
            }
        }
        if (errString != null) {
            sessionThread.writeString(errString);
        } else {
            sessionThread.writeString("200 OPTS accepted\r\n");
            Log.d(TAG, "Handled OPTS ok");
        }
    }

}
