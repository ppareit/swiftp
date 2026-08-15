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
package be.ppareit.swiftp.tasker

import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.FsSettings
import com.joaomgcd.taskerpluginlibrary.output.TaskerOutputObject
import com.joaomgcd.taskerpluginlibrary.output.TaskerOutputVariable

/**
 * What we hand back to the host, available there as %ftp_running and %ftp_url.
 */
@TaskerOutputObject
class ServerStateOutput(
        @get:TaskerOutputVariable("ftp_running") val running: Boolean,
        @get:TaskerOutputVariable("ftp_url") val url: String
) {
    companion object {
        /** Snapshot of the server as it is right now. */
        fun current(): ServerStateOutput {
            val running = FsService.isRunning()
            val address = FsService.getLocalInetAddress()
            // Same shape as the notification and the preference screen.
            val url = if (running && address != null) {
                "ftp://" + address.hostAddress + ":" + FsSettings.getPortNumber()
            } else {
                ""
            }
            return ServerStateOutput(running, url)
        }
    }
}
