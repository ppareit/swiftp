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
import android.view.View.OnClickListener;
import android.widget.Button;
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
    private Button addUserButton;
    private Button manageUsersButton;
    private Button serverOptionsButton;
    
    private TextView ipText;
	
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
        	
        startStopButton = (Button) findViewById(R.id.start_stop_button);
        addUserButton = (Button) findViewById(R.id.add_user_button);
        manageUsersButton = (Button) findViewById(R.id.manage_users_button);
        serverOptionsButton = (Button) findViewById(R.id.server_options_button);
        
        startStopButton.setOnClickListener(startStopListener);
        addUserButton.setOnClickListener(addUserListener);
        manageUsersButton.setOnClickListener(manageUsersListener);
        serverOptionsButton.setOnClickListener(serverOptionsListener);
        
        
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
    	// Update the start/stop button to show the correct text
    	if(FTPServerService.isRunning()) {
    		startStopButton.setText(R.string.stop_server);
    	} else {
    		startStopButton.setText(R.string.start_server);
    	}
    	String ip =  FTPServerService.getWifiIpAsString();
    	if(ip != null) {
    		ipText.setText("My URL on Wifi: ftp://" + ip + 
    		               ":" + FTPServerService.PORT + "/");
    	} else {
    		ipText.setText("Wifi is not enabled");
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

    /**
     * A call-back for when the user presses the back button.
     */
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
        	if(startStopButton.getText().equals(startString)) {
    			/* The button had the "start server" text  */
        		context.startService(intent);
        	} else {
        		/* The button didn't have the "start server" text so it must
        		 * have had the "stop server" text. We stop it now.
        		 */
        		context.stopService(intent);
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
}
