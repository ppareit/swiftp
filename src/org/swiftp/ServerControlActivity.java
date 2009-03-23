/*
 * Copyright (C) 2007 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.swiftp;

import java.net.InetAddress;
import java.util.List;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.TextView;

/**
 * This class provides a basic demonstration of how to write an Android
 * activity. Inside of its window, it places a single view: an EditText that
 * displays and edits some internal text.
 */
public class ServerControlActivity extends Activity {
    
    static final private int BACK_ID = Menu.FIRST;
    static final private int CLEAR_ID = Menu.FIRST + 1;

    //static final private String LOGTAG = "SwiFTP control";
    //private EditText mEditor;
    //private ListView actionList;
    
    //private int mPosStartStop;
    //private int mPosAddUser;
    //private int mPosManageUsers;
    
    private Button startStopButton;
    //private Button addUserButton;
    //private Button manageUsersButton;
    //private Button serverOptionsButton;

    private TextView statusText;
    private TextView ipText;
	
    private TextView sessionMonitor;
    private CheckBox sessionMonitorCheckBox;
    private TextView serverLog;
    private CheckBox serverLogCheckBox;
    
    protected MyLog myLog = new MyLog(this.getClass().getName());
    
    public Handler handler = new Handler() {
    	public void handleMessage(Message msg) {
    		updateUi();
    	}
    };
    
    public ServerControlActivity() {
    	
    }

    /** Called with the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // Set the application-wide context global, if not already set
		Context myContext = Globals.getContext();
		if(myContext == null) {
			myContext = getApplicationContext();
			if(myContext == null) {
				throw new NullPointerException("Null context!?!?!?");
			}
			Globals.setContext(myContext);
		}
        // Inflate our UI from its XML layout description.
        setContentView(R.layout.server_control_activity);
        
        ipText = (TextView)findViewById(R.id.ip_address);
        statusText = (TextView)findViewById(R.id.server_status);
        	
        startStopButton = (Button) findViewById(R.id.start_stop_button);
        //addUserButton = (Button) findViewById(R.id.add_user_button);
        //manageUsersButton = (Button) findViewById(R.id.manage_users_button);
        //serverOptionsButton = (Button) findViewById(R.id.server_options_button);
        
        startStopButton.setOnClickListener(startStopListener);
        //addUserButton.setOnClickListener(addUserListener);
        //manageUsersButton.setOnClickListener(manageUsersListener);
        //serverOptionsButton.setOnClickListener(serverOptionsListener);
        
        sessionMonitor = (TextView) findViewById(R.id.session_monitor);
        sessionMonitorCheckBox = 
        	(CheckBox) findViewById(R.id.session_monitor_checkbox);
        serverLog = (TextView) findViewById(R.id.server_log);
        serverLogCheckBox = (CheckBox) findViewById(R.id.server_log_checkbox);
        
        //sessionMonitor.setHeight(1);
        //serverLog.setHeight(1);
        
        sessionMonitorCheckBox
        	.setOnClickListener(sessionMonitorCheckBoxListener);
        serverLogCheckBox.setOnClickListener(serverLogCheckBoxListener);
        
        updateUi();
        //((Button) findViewById(R.id.clear)).setOnClickListener(mClearListener);
        
        //mEditor.setText(getText(R.string.main_label));
        
//        ArrayList<String> actionChoices = new ArrayList<String>();
//        actionChoices.add("@string/start_stop");
//        mPosStartStop = 0;
//        actionChoices.add("@string/add_user");
//        actionChoices.add("@string/manage_users");
        
        /*ArrayList<TextView> actionChoices = new ArrayList<TextView>();
        TextView temp = new TextView
        actionChoices.add(new TextView(this).setText(R.string.start_stop));
        mPosStartStop = 0;
        actionChoices.add(new TextView(this).setText(R.string.add_user));
        actionChoices.add(new TextView(this).setText(R.string.manage_users));
        
        
        actionList = (ListView) findViewById(R.id.action_list);
        ArrayAdapter<String> actionListAdapter;
        actionListAdapter = new ArrayAdapter<String>(
        		this,
        		android.R.layout.simple_list_item_1, actionChoices);
        
        
        actionList.setAdapter(actionListAdapter);*/
        
    }


    /**
     * Whenever we regain focus, we should update the button text depending
     * on the state of the server service.
     */
    protected void onStart() {
    	super.onStart();
		UiUpdater.registerClient(handler);
		updateUi();
    }
    
    protected void onResume() {
    	super.onResume();
		UiUpdater.registerClient(handler);
		updateUi();
    }

    /* Whenever we lose focus, we must unregister from UI update messages from
     * the FTPServerService, because we may be deallocated.
     */
    protected void onPause() {
    	super.onPause();
		UiUpdater.unregisterClient(handler);
    }
    
    protected void onStop() {
    	super.onStop();
    	UiUpdater.unregisterClient(handler);
    }
    
    protected void onDestroy() {
    	super.onDestroy();
    	UiUpdater.unregisterClient(handler);
    }
    
    /**
     * This will be called by the static UiUpdater whenever the service has
     * changed state in a way that requires us to update our UI.
     */
    public void updateUi() {
    	if(FTPServerService.isRunning()) {
    		InetAddress address =  FTPServerService.getServerAddress();
        	if(address != null) {
    			ipText.setText("ftp://" + address.getHostAddress() + 
    		               ":" + FTPServerService.PORT + "/");
        	} else {
        		ipText.setText(R.string.cant_get_url);
        	}
        	startStopButton.setText(R.string.stop_server);
        	statusText.setText(R.string.running);
    	} else {
    		// Update the start/stop button to show the correct text
    		if(FTPServerService.isWifiEnabled()) {
    			startStopButton.setText(R.string.start_server);
    		} else {
    			startStopButton.setText(R.string.cannot_start_until_wifi);
    		}
    		ipText.setText(R.string.no_url_yet);
    		statusText.setText(R.string.stopped);
    	}
    	if(sessionMonitorCheckBox.isChecked()) {
    		// If the session monitor is visible, then retrieve the contents
    		// from the FTPServerService
    		sessionMonitor.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);
    		List<String> lines = FTPServerService.getSessionMonitorContents();
			int size = Settings.getSessionMonitorScrollBack();
    		sessionMonitor.setMinLines(size);
    		sessionMonitor.setMaxLines(size);
    		String showText = "";
    		for(String line : lines) {
    			showText += showText + line + "\n";  
    		}
    		sessionMonitor.setText(showText);
    	} else {
    		sessionMonitor.setHeight(1);
    	}
    	if(serverLogCheckBox.isChecked()) {
    		// If the server log is visible, then retrieve the contents
    		// from the FTPServerService
    		serverLog.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);
    		List<String> lines = FTPServerService.getServerLogContents();
    		//Log.d("", "Got " + lines.size() + " lines from server");
			int size = Settings.getServerLogScrollBack();
    		serverLog.setMinLines(size);
    		serverLog.setMaxLines(size);
    		String showText = "";
    		for(String line : lines) {
    			showText = showText + line + "\n";  
    		}
    		serverLog.setText(showText);
    	} else {
    		serverLog.setHeight(1);
    	}
    }
    
    /**
     * Called when your activity's options menu needs to be created.
     */
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);

        // We are going to create two menus. Note that we assign them
        // unique integer IDs, labels from our string resources, and
        // given them shortcuts.
        menu.add(0, BACK_ID, 0, R.string.back).setShortcut('0', 'b');
        menu.add(0, CLEAR_ID, 0, R.string.clear).setShortcut('1', 'c');

        return true;
    }

    /**
     * Called right before your activity's option menu is displayed.
     */
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        super.onPrepareOptionsMenu(menu);

        // Before showing the menu, we need to decide whether the clear
        // item is enabled depending on whether there is text to clear.
        //menu.findItem(CLEAR_ID).setVisible(mEditor.getText().length() > 0);

        return true;
    }

    /**
     * Called when a menu item is selected.
     */
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        /*switch (item.getItemId()) {
        case BACK_ID:
            finish();
            return true;
        case CLEAR_ID:
            mEditor.setText("");
            return true;
        }*/

        return super.onOptionsItemSelected(item);
    }

    OnClickListener startStopListener = new OnClickListener() {
        public void onClick(View v) {
    		Context context = getApplicationContext();
    		Intent intent = new Intent(context,	FTPServerService.class);
    		/*
        	 * In order to choose whether to stop or start the server, we check
        	 * the text on the button to see which action the user was 
        	 * expecting.
        	 */
    		Resources resources = getResources(); // fetch app resources
    		String startString = resources.getString(R.string.start_server);
    		String stopString = resources.getString(R.string.stop_server);
    		String noWifiString = resources.getString(R.string.cannot_start_until_wifi);
    		String buttonText = startStopButton.getText().toString(); 
        	if(buttonText.equals(startString)) { 
    			/* The button had the "start server" text  */
        		context.startService(intent);
        	} else if (buttonText.equals(stopString)) {
        		/* The button had the "stop server" text. We stop the server now. */
        		context.stopService(intent);
        	} else if (buttonText.equals(noWifiString)) {
        		// If we were waiting for the user to enable wifi, then just do a
        		// refresh of the wifi state when they press the start/stop button
        		updateUi();
        	} else {
        		// Do nothing
        		myLog.l(Log.ERROR, "Unrecognized start/stop text");
        	}
        }
    };

    OnClickListener addUserListener = new OnClickListener() {
        public void onClick(View v) {
        	myLog.l(Log.INFO, "Add user stub");
        }
    };

    OnClickListener manageUsersListener = new OnClickListener() {
        public void onClick(View v) {
        	myLog.l(Log.INFO, "Manage users stub");
        }
    };

    OnClickListener serverOptionsListener = new OnClickListener() {
        public void onClick(View v) {
        	myLog.l(Log.INFO, "Server options stub");
        }
    };

    
    /**
     * A call-back for when the user presses the clear button.
     */
    OnClickListener mClearListener = new OnClickListener() {
        public void onClick(View v) {
        	
        }
    };
    
    /**
     * A callback for when the user toggles the session monitor on or off
     */
    OnClickListener sessionMonitorCheckBoxListener = new OnClickListener() {
        public void onClick(View v) {
        	// Trigger a UI update message to our Activity
            UiUpdater.updateClients();
        	//updateUi();
        }
    };

    /**
     * A callback for when the user toggles the server log on or off
     */
    OnClickListener serverLogCheckBoxListener = new OnClickListener() {
        public void onClick(View v) {
        	// Trigger a UI update message to our Activity
        	UiUpdater.updateClients();
            //updateUi();
        }
    };

}
