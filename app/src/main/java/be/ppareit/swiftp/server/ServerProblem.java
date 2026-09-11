/*
Copyright 2026 Pieter Pareit

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

import java.util.Locale;

/**
 * Everything that can go wrong on the server that the user on the device can fix.
 * One list, said twice: {@link #message} is the line a client gets in the FTP reply, which is
 * protocol and therefore English, and the settings screen has the same list in the string
 * table, in the language of the person who has to act on it.
 *
 * The numbers and addresses go in as arguments, in the order each case documents below, and
 * fill the same holes in both. Never build a sentence out here: the server package has no
 * Context and no resources, which is exactly why the wording is a template and not a String
 * assembled at the failure.
 *
 * @see be.ppareit.swiftp.FsService#reportProblem
 * @see be.ppareit.swiftp.gui.PreferenceFragment#messageOf
 */
public enum ServerProblem {

    /** A data connection could not be opened and nothing more is known about it. */
    DATA_SOCKET("Error opening data socket"),

    /** A transfer was asked for before any PASV or PORT. No arguments. */
    DATA_NOT_SET_UP("No data connection set up, send PASV or PORT first"),

    /** Active mode: the client's own address would not take the connection. Host, port. */
    DATA_CONNECT_FAILED("Could not connect data socket to %s port %s"),

    /** Active mode: the connection was made, the TLS handshake was not. Host, port. */
    DATA_TLS_FAILED_ACTIVE("TLS handshake failed on data connection to %s port %s"),

    /** Passive mode: nothing arrived on the announced port. Port, seconds waited. */
    DATA_NO_CONNECTION("No data connection on port %s after %ss"),

    /** Passive mode: accepting on the announced port failed. Port. */
    DATA_ACCEPT_FAILED("Error opening data socket on port %s"),

    /** Passive mode: somebody arrived, the TLS handshake failed. Port. */
    DATA_TLS_FAILED_PASSIVE("TLS handshake failed on data connection on port %s"),

    /** A login was refused because the SAF path is in use and no folder has been granted. */
    NOTHING_SHARED("No folders are shared. Open SwiFTP on the device and go to Allowed folders."),

    /** The server runs, but there is no user and no anonymous login, so nobody can log in. */
    NO_USERS("No users. Open SwiFTP on the device and go to Manage users."),

    /** A login was refused because the folder that user is kept in is not shared. Username. */
    USER_FOLDER_NOT_SHARED("The folder for %s is not shared. Open SwiFTP on the device and go to"
            + " Manage users."),
    ;

    private final String template;

    ServerProblem(String template) {
        this.template = template;
    }

    /**
     * The English line for the FTP reply and the log, with the arguments this case documents.
     * Locale.US because it goes on the wire: a device set to a language that groups digits
     * would otherwise put a separator in a port number.
     */
    public String message(Object... args) {
        return args.length == 0 ? template : String.format(Locale.US, template, args);
    }
}
