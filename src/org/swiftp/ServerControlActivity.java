

package org.swiftp;

import java.net.InetAddress;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
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

public class ServerControlActivity extends Activity {
    
    private Button startStopButton;
    //private Button addUserButton;
    //private Button manageUsersButton;
    //private Button serverOptionsButton;
    private Button instructionsButton;
    
    private TextView statusText;
    private TextView ipText;
	
    private TextView sessionMonitor;
    private CheckBox sessionMonitorCheckBox;
    private TextView serverLog;
    private CheckBox serverLogCheckBox;
    
    protected MyLog myLog = new MyLog(this.getClass().getName());
    
    protected Context activityContext = this;
    
    public Handler handler = new Handler() {
    	public void handleMessage(Message msg) {
    		// If more than one update is queued up, we only need to do one.
    		// The call to removeMessages clears all messages with the value
    		// of 0, which all of our update messages have.
    		removeMessages(0); 
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
        instructionsButton = (Button) findViewById(R.id.instructions);
        
        startStopButton.setOnClickListener(startStopListener);
        //addUserButton.setOnClickListener(addUserListener);
        //manageUsersButton.setOnClickListener(manageUsersListener);
        //serverOptionsButton.setOnClickListener(serverOptionsListener);
        instructionsButton.setOnClickListener(instructionsListener);
        
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
    	Log.d("manual", "In updateUi()");
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
