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

import android.app.Activity
import android.content.Context
import android.os.Bundle
import be.ppareit.swiftp.FsService
import com.joaomgcd.taskerpluginlibrary.condition.TaskerPluginRunnerConditionNoOutputOrInputOrUpdateState
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfigHelperStateNoOutputOrInputOrUpdate
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfigNoInput
import com.joaomgcd.taskerpluginlibrary.input.TaskerInput
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultCondition
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultConditionSatisfied
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultConditionUnsatisfied

/**
 * State: the host asks whether the server is running right now, so tasks can be
 * written as "if FTP server is running, then ...".
 */
class StateRunnerServerRunning : TaskerPluginRunnerConditionNoOutputOrInputOrUpdateState() {

    override fun getSatisfiedCondition(
            context: Context,
            input: TaskerInput<Unit>,
            update: Unit?
    ): TaskerPluginResultCondition<Unit> {
        return if (FsService.isRunning()) {
            TaskerPluginResultConditionSatisfied(context, Unit)
        } else {
            TaskerPluginResultConditionUnsatisfied()
        }
    }
}

class StateHelperServerRunning(config: TaskerPluginConfigNoInput) :
        TaskerPluginConfigHelperStateNoOutputOrInputOrUpdate<StateRunnerServerRunning>(config) {

    override val runnerClass get() = StateRunnerServerRunning::class.java
}

/**
 * There is nothing to configure, so this closes itself straight away.
 */
class StateConfigServerRunning : Activity(), TaskerPluginConfigNoInput {

    override val context: Context get() = applicationContext

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        StateHelperServerRunning(this).finishForTasker()
    }
}
