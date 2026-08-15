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

import com.joaomgcd.taskerpluginlibrary.input.TaskerInputField
import com.joaomgcd.taskerpluginlibrary.input.TaskerInputRoot

/**
 * What the host asks us to do: run the server, or stop it.
 *
 * The no argument constructor is required: the plugin library instantiates this
 * reflectively and throws NoEmptyConstructorException without it.
 */
@TaskerInputRoot
class ServerStateInput @JvmOverloads constructor(
        @field:TaskerInputField("running") var running: Boolean = true
)
