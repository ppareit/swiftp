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

/**
 * Something that went wrong on the server. The server names the case, the settings screen
 * owns the sentence, so the wording stays in the string table and can be translated. Anything
 * the server alone knows, a port or a cause, travels beside it as a detail line.
 *
 * @see be.ppareit.swiftp.FsService#reportProblem
 */
public enum ServerProblem {
    /** A passive data connection could not be opened. The detail says which port and why. */
    DATA_SOCKET,
    /** A login was refused because the SAF path is in use and no folder has been granted. */
    NOTHING_SHARED,
}
