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

import android.content.Context
import android.content.pm.PackageManager
import android.os.Bundle
import android.view.MenuItem
import android.widget.RadioGroup
import androidx.activity.OnBackPressedCallback
import androidx.appcompat.app.AppCompatActivity
import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.R
import com.joaomgcd.taskerpluginlibrary.action.TaskerPluginRunnerActionNoOutput
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfig
import com.joaomgcd.taskerpluginlibrary.config.TaskerPluginConfigHelperNoOutput
import com.joaomgcd.taskerpluginlibrary.input.TaskerInput
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResult
import com.joaomgcd.taskerpluginlibrary.runner.TaskerPluginResultSucess
import net.vrallev.android.cat.Cat

/**
 * Action: the host tells us to start or stop the server.
 */
class ActionRunnerSetServerState : TaskerPluginRunnerActionNoOutput<ServerStateInput>() {

    override fun run(
            context: Context,
            input: TaskerInput<ServerStateInput>
    ): TaskerPluginResult<Unit> {
        val running = input.regular.running
        if (running && !FsService.isRunning()) {
            FsService.start()
        } else if (!running && FsService.isRunning()) {
            FsService.stop()
        }
        return TaskerPluginResultSucess()
    }
}

class ActionHelperSetServerState(config: TaskerPluginConfig<ServerStateInput>) :
        TaskerPluginConfigHelperNoOutput<ServerStateInput, ActionRunnerSetServerState>(config) {

    override val inputClass get() = ServerStateInput::class.java
    override val runnerClass get() = ActionRunnerSetServerState::class.java
}

/**
 * The screen the host opens when the user adds this action to a task.
 */
class ActionConfigSetServerState : AppCompatActivity(), TaskerPluginConfig<ServerStateInput> {

    private val helper by lazy { ActionHelperSetServerState(this) }

    override val context: Context get() = applicationContext

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.locale_edit_layout)

        // Title the screen with whatever app is asking, as the old plugin did.
        try {
            val pm = packageManager
            callingPackage?.let {
                title = pm.getApplicationLabel(pm.getApplicationInfo(it, 0))
            }
        } catch (e: PackageManager.NameNotFoundException) {
            Cat.e("Calling package couldn't be found%s", e)
        }
        supportActionBar?.apply {
            setSubtitle(R.string.swiftp_name)
            setDisplayHomeAsUpEnabled(true)
        }

        helper.onCreate()

        // Leaving the screen is what saves the setting back to the host, so the
        // helper gets to veto the exit if the input is not valid.
        onBackPressedDispatcher.addCallback(this, object : OnBackPressedCallback(true) {
            override fun handleOnBackPressed() {
                if (helper.onBackPressed().success) {
                    isEnabled = false
                    onBackPressedDispatcher.onBackPressed()
                }
            }
        })
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        if (item.itemId == android.R.id.home) {
            // Treat the up arrow as "done", matching the back button.
            onBackPressedDispatcher.onBackPressed()
            return true
        }
        return super.onOptionsItemSelected(item)
    }

    override fun assignFromInput(input: TaskerInput<ServerStateInput>) {
        val selected = if (input.regular.running) {
            R.id.radio_server_running
        } else {
            R.id.radio_server_stopped
        }
        findViewById<RadioGroup>(R.id.radio_server_state_group).check(selected)
    }

    override val inputForTasker: TaskerInput<ServerStateInput>
        get() {
            val checked = findViewById<RadioGroup>(R.id.radio_server_state_group)
                    .checkedRadioButtonId
            return TaskerInput(ServerStateInput(checked == R.id.radio_server_running))
        }
}
