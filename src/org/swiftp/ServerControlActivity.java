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

package org.swiftp;

import java.net.InetAddress;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.view.Gravity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.TextView;
import android.widget.Toast;

public class ServerControlActivity extends Activity {
    
    private Button startStopButton;
    //private Button addUserButton;
    //private Button manageUsersButton;
    //private Button serverOptionsButton;
    private Button instructionsButton;
    private Button setupButton;
    private Button wifiButton;
    
    private TextView wifiStatusText;
    private TextView serverStatusText;
    private TextView ipText;
    private TextView lastErrorText;
    
    private TextView sessionMonitor;
    private CheckBox sessionMonitorCheckBox;
    private TextView serverLog;
    private CheckBox serverLogCheckBox;
    
    protected MyLog myLog = new MyLog(this.getClass().getName());
    
    protected Context activityContext = this;
    
    public Handler handler = new Handler() {
    	public void handleMessage(Message msg) {
    		switch(msg.what) {
    		case 0:  // We are being told to do a UI update
    			// If more than one UI update is queued up, we only need to do one.
    			removeMessages(0);
    			updateUi();
    			break;
    		case 1:  // We are being told to display an error message
    			removeMessages(1);
    		}
    	
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
        
        ipText =           (TextView)findViewById(R.id.ip_address);
        serverStatusText = (TextView)findViewById(R.id.server_status);
        wifiStatusText =   (TextView)findViewById(R.id.wifi_status);
        lastErrorText =    (TextView)findViewById(R.id.last_error);
        
        startStopButton = (Button) findViewById(R.id.start_stop_button);
        //addUserButton = (Button) findViewById(R.id.add_user_button);
        //manageUsersButton = (Button) findViewById(R.id.manage_users_button);
        //serverOptionsButton = (Button) findViewById(R.id.server_options_button);
        instructionsButton = (Button) findViewById(R.id.instructions);
        setupButton = (Button) findViewById(R.id.setup);
        wifiButton = (Button)findViewById(R.id.wifi_button);
        
        startStopButton.setOnClickListener(startStopListener);
        //addUserButton.setOnClickListener(addUserListener);
        //manageUsersButton.setOnClickListener(manageUsersListener);
        //serverOptionsButton.setOnClickListener(serverOptionsListener);
        instructionsButton.setOnClickListener(instructionsListener);
        setupButton.setOnClickListener(setupListener);
        wifiButton.setOnClickListener(wifiButtonListener);
        
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

        // If the required preferences are not present, launch the configuration
        // Activity.
//        SharedPreferences settings = getSharedPreferences(
//        		Defaults.getSettingsName(),	Defaults.getSettingsMode());
//		String username = settings.getString("username", null);
//		String password = settings.getString("password", null);
//		if(username == null || password == null) {
//			launchConfigureActivity();
//		}
//        updateUi();
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
    	// If the required preferences are not present, launch the configuration
        // Activity.
        if(!requiredSettingsDefined()) {
        	launchConfigureActivity();
        }
        UiUpdater.registerClient(handler);
		updateUi();
		// Register to receive wifi status broadcasts
		myLog.l(Log.DEBUG, "Registered for wifi updates");
		this.registerReceiver(wifiReceiver, 
				new IntentFilter(WifiManager.WIFI_STATE_CHANGED_ACTION));
    }

    /* Whenever we lose focus, we must unregister from UI update messages from
     * the FTPServerService, because we may be deallocated.
     */
    protected void onPause() {
    	super.onPause();
		UiUpdater.unregisterClient(handler);
		myLog.l(Log.DEBUG, "Unregistered for wifi updates");
		this.unregisterReceiver(wifiReceiver);
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
     * 
     * We can't use any myLog.l() calls in this function, because that will
     * trigger an endless loop of UI updates.
     */
    public void updateUi() {
    	WifiManager wifiMgr = (WifiManager)getSystemService(Context.WIFI_SERVICE);
    	int wifiState = wifiMgr.getWifiState();
    	
    	// Set the start/stop button text and server status text
    	if(FTPServerService.isRunning()) {
    		InetAddress address =  FTPServerService.getServerAddress();
        	if(address != null) {
        		ipText.setText("ftp://" + address.getHostAddress() + 
    		               ":" + FTPServerService.getPort() + "/");
        	} else {
        		ipText.setText(R.string.cant_get_url);
        	}
        	startStopButton.setVisibility(View.VISIBLE);
        	startStopButton.setText(R.string.stop_server);
        	serverStatusText.setText(R.string.running);
    	} else {
    		// Update the start/stop button to show the correct text
    		ipText.setText(R.string.no_url_yet);
    		serverStatusText.setText(R.string.stopped);
    		startStopButton.setText(R.string.start_server);
    	}

    	// Manage the visibility of the start/stop button based on wifi state
    	if(wifiState == WifiManager.WIFI_STATE_ENABLED) {
			//myLog.l(Log.DEBUG, "Showing start/stop button due to enabled wifi");
			startStopButton.setVisibility(View.VISIBLE);
		} else {
			// Only hide the button if the server is not running. If it is
			// running, the button should be left visible so the server
			// can be stopped, regardless of wifi state.
			if(!FTPServerService.isRunning()) {
				//myLog.l(Log.DEBUG, "Hiding start/stop button due to disabled wifi");
				startStopButton.setVisibility(View.GONE);
			} else {
				//myLog.l(Log.DEBUG, "Would hide startStopButton but running");
			}
		}
    	
    	// Manage the text of the wifi enable/disable button and the 
    	// wifi status text.
    	switch(wifiState) {
    	case WifiManager.WIFI_STATE_ENABLED:
    		wifiButton.setText(R.string.disable_wifi);
    		wifiStatusText.setText(R.string.enabled);
    		break;
    	case WifiManager.WIFI_STATE_DISABLED:
    		wifiButton.setText(R.string.enable_wifi);
    		wifiStatusText.setText(R.string.disabled);
    		break;
    	default:
    		// We're in some transient state that will eventually
    		// become one of the other two states.
    		wifiStatusText.setText(R.string.waiting);
    		break;
    	}

    	// Manage the visibility and text of the "last error" display
    	// and popup a dialog box, if there has been an error
    	String errString;
    	if((errString = Globals.getLastError()) != null) {
    		Globals.setLastError(null);  // Clear the error condition after retrieving
    		lastErrorText.setText(errString);
    		lastErrorText.setVisibility(View.VISIBLE);
    		TextView lastErrorLabel = (TextView)findViewById(R.id.last_error_label);
    		lastErrorLabel.setVisibility(View.VISIBLE);
    		
    		//TextView textView = new TextView(this);
        	//textView.setText(R.string.error_dialog_text);
        	
        	AlertDialog dialog =  new AlertDialog.Builder(this).create();
        	CharSequence text = getText(R.string.error_dialog_text);
        	String str = text.toString().replace("%%%Replace_Here%%%", errString);
        	//text = text + "\n\n" + getText(R.string.the_error_was) + "\n\n" 
        	//	+ errString;
        	dialog.setMessage(str);
        	dialog.setTitle(getText(R.string.error_dialog_label));
        	dialog.setButton(getText(R.string.ok), ignoreDialogListener);
        	dialog.show();
        	
    	}
    	
    	
		// If the session monitor is enabled, then retrieve the contents
		// from the FTPServerService
    	if(sessionMonitorCheckBox.isChecked()) {
    		sessionMonitor.setHeight(ViewGroup.LayoutParams.WRAP_CONTENT);
    		List<String> lines = FTPServerService.getSessionMonitorContents();
			int size = Defaults.getSessionMonitorScrollBack();
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
			int size = Defaults.getServerLogScrollBack();
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
        return true;
    }

    /**
     * Called right before your activity's option menu is displayed.
     */
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        super.onPrepareOptionsMenu(menu);
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
    		String buttonText = startStopButton.getText().toString(); 
        	if(buttonText.equals(startString)) { 
    			/* The button had the "start server" text  */
        		context.startService(intent);
        	} else if (buttonText.equals(stopString)) {
        		/* The button had the "stop server" text. We stop the server now. */
        		context.stopService(intent);
        	} else {
        		// Do nothing
        		myLog.l(Log.ERROR, "Unrecognized start/stop text");
        	}
        }
    };

    OnClickListener wifiButtonListener = new OnClickListener() {
        public void onClick(View v) {
        	Resources resources = getResources(); // fetch app resources
        	String enableString = resources.getString(R.string.enable_wifi);
        	String disableString = resources.getString(R.string.disable_wifi);
        	
        	String buttonText = wifiButton.getText().toString();
    		WifiManager wifi = (WifiManager) getSystemService(Context.WIFI_SERVICE);
        	
        	if (buttonText.equals(enableString)) {
        		myLog.l(Log.INFO, "User requested enable wifi");
        		wifi.setWifiEnabled(true);
        		wifiButton.setText(R.string.wifi_waiting);
        	} else if (buttonText.equals(disableString)){
        		myLog.l(Log.INFO, "User requested disable wifi");
        		wifiButton.setText(R.string.wifi_waiting);
        		wifi.setWifiEnabled(false);
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
    
    DialogInterface.OnClickListener ignoreDialogListener = 
    	new DialogInterface.OnClickListener() 
    {
    	public void onClick(DialogInterface dialog, int which) {
    	}
    };
    
    
    /**
     * A call-back for when the user presses the instructions button.
     */
    OnClickListener instructionsListener = new OnClickListener() {
        public void onClick(View v) {
        	
        	TextView textView = new TextView(getApplicationContext());
        	textView.setText(R.string.instructions_text);
        	
        	AlertDialog dialog =  new AlertDialog.Builder(activityContext).create();
        	CharSequence instructions = getText(R.string.instructions_text);
        	dialog.setMessage(instructions);
        	dialog.setTitle(getText(R.string.instructions_label));
        	dialog.setButton(getText(R.string.ok), ignoreDialogListener);
        	dialog.show();
        	
        }
    };
    
    /**
     * A call-back for when the user presses the "setup" button.
     */
    OnClickListener setupListener = new OnClickListener() {
        public void onClick(View v) {
        	launchConfigureActivity();
        }
    };
    
    void launchConfigureActivity() {
    	if(!requiredSettingsDefined()) {
	    	Toast toast = Toast.makeText(this, R.string.must_config,
					Toast.LENGTH_SHORT);
	    	toast.setGravity(Gravity.CENTER, 0, 0);
	    	toast.show();
    	}
    	Intent intent = new Intent(activityContext, ConfigureActivity.class);
    	startActivity(intent);
    }
    
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
    
    BroadcastReceiver wifiReceiver = new BroadcastReceiver() {
    	public void onReceive(Context ctx, Intent intent) {
        	myLog.l(Log.DEBUG, "Wifi status broadcast received");
    		updateUi();
    	}
    };
    
    boolean requiredSettingsDefined() {
    	SharedPreferences settings = getSharedPreferences(
        		Defaults.getSettingsName(),	Defaults.getSettingsMode());
		String username = settings.getString("username", null);
		String password = settings.getString("password", null);
		if(username == null || password == null) {
			return false;
		} else {
			return true;
		}
    }

}
