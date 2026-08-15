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
import android.content.BroadcastReceiver
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.Bundle
import com.joaomgcd.taskerpluginlibrary.TaskerPluginConstants
import com.joaomgcd.taskerpluginlibrary.condition.TaskerPluginRunnerConditionEvent
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfig
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfigHelper
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfigNoInput
import com.joaomgcd.taskerpluginlibrary.input.TaskerInput
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultCondition
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultConditionSatisfied
import net.dinglisch.android.tasker.TaskerPlugin
import net.vrallev.android.cat.Cat

/**
 * Event: we tell the host that the server started or stopped.
 */
class EventRunnerServerStateChanged :
        TaskerPluginRunnerConditionEvent<Unit, ServerStateOutput, Unit>() {

    override fun getSatisfiedCondition(
            context: Context,
            input: TaskerInput<Unit>,
            update: Unit?
    ): TaskerPluginResultCondition<ServerStateOutput> {
        return TaskerPluginResultConditionSatisfied(context, ServerStateOutput.current())
    }
}

class EventHelperServerStateChanged(config: TaskerPluginConfig<Unit>) :
        TaskerPluginConfigHelper<Unit, ServerStateOutput, EventRunnerServerStateChanged>(config) {

    override val inputClass get() = Unit::class.java
    override val outputClass get() = ServerStateOutput::class.java
    override val runnerClass get() = EventRunnerServerStateChanged::class.java
}

/**
 * Nothing to configure, so this closes itself straight away.
 */
class EventConfigServerStateChanged : Activity(), TaskerPluginConfigNoInput {

    override val context: Context get() = applicationContext

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        EventHelperServerStateChanged(this).finishForTasker()
    }
}

/**
 * Relays the broadcasts FsService already sends when it starts, stops or fails
 * to start. Registered at runtime from App, alongside the other listeners for
 * those same actions.
 */
class ServerStateBroadcastReceiver : BroadcastReceiver() {

    override fun onReceive(context: Context, intent: Intent) {
        Cat.d("Received broadcast: " + intent.action + ", notifying plugin host")
        requestQuery(context, StateConfigServerRunning::class.java)
        requestQuery(context, EventConfigServerStateChanged::class.java)
    }

    private fun requestQuery(context: Context, configActivity: Class<out Activity>) {
        val request = Intent(TaskerPluginConstants.ACTION_REQUEST_QUERY).apply {
            addFlags(Intent.FLAG_RECEIVER_FOREGROUND)
            putExtra(TaskerPluginConstants.EXTRA_ACTIVITY, configActivity.name)
            TaskerPlugin.Event.addPassThroughMessageID(this)
        }
        val hosts = context.packageManager.queryBroadcastReceivers(request, 0)
        if (hosts.isEmpty()) {
            Cat.d("No plugin host listens for " + TaskerPluginConstants.ACTION_REQUEST_QUERY)
            return
        }
        for (host in hosts) {
            val receiver = host.activityInfo
            context.sendBroadcast(Intent(request).setComponent(
                    ComponentName(receiver.packageName, receiver.name)))
        }
    }
}
