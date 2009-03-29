package org.swiftp;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;

public class ConfigureActivity extends Activity implements OnClickListener {
	private EditText usernameBox;
	private EditText passwordBox;
	private EditText portNumberBox;
	
	private Button saveButton;
	
    public ConfigureActivity() {
	}
	
	public void onCreate(Bundle savedInstance) {
		super.onCreate(savedInstance);
		
		SharedPreferences settings = null;
		// Inflate our UI from its XML layout description.
        setContentView(R.layout.configure_activity);
		
        saveButton = (Button) findViewById(R.id.config_save);
        saveButton.setOnClickListener(this);
        
        usernameBox = (EditText) findViewById(R.id.config_username);
        passwordBox = (EditText) findViewById(R.id.config_password);
        portNumberBox = (EditText) findViewById(R.id.config_portnum);
        
		settings = getSharedPreferences(Defaults.getSettingsName(),
				Defaults.getSettingsMode());
		
		String username = settings.getString("username", "");
		String password = settings.getString("password", "");
		int portNumber = settings.getInt("portNum", Defaults.getPortNumber());
		
		usernameBox.setText(username);
		passwordBox.setText(password);
		portNumberBox.setText(Integer.toString(portNumber));
	}
	
	protected void onStart() {
		super.onStart();
	}
	
	protected void onResume() {
		super.onResume();
	}
	
	protected void onPause() {
    	super.onPause();
    }
    
    protected void onStop() {
    	super.onStop();
    }
    
    protected void onDestroy() {
    	super.onDestroy();
    }

	public void onClick(View v) {
		// This function is called when the user clicks "save."
		
		// Let's validate all the input fields.
		String errString = null;
		int portNum = 0;
		String username, password, portNumberString;
		username = usernameBox.getText().toString();
		password = passwordBox.getText().toString();
		portNumberString = portNumberBox.getText().toString();
		
		validateBlock: {
			if(!username.matches("[a-zA-Z0-9]+")) {
				errString = getString(R.string.username_validation_error);
				break validateBlock;
			}
			if (!password.matches("[a-zA-Z0-9]+")){
				errString = getString(R.string.password_validation_error); 
				break validateBlock;
			}

			try {
				portNum = Integer.parseInt(portNumberString);
			} catch (Exception e) {
				portNum = 0;
			}
			if(portNum <= 1024 || portNum > 65535) {
				errString = getString(R.string.port_validation_error);
				break validateBlock;
			}
		}
		if(errString != null) {
			AlertDialog dialog =  new AlertDialog.Builder(this).create();
        	dialog.setMessage(errString);
        	dialog.setTitle(getText(R.string.instructions_label));
        	// This whole mess just adds a do-nothing "OK" button to the dialog
        	dialog.setButton(getString(R.string.ok), new DialogInterface.OnClickListener() {
        		public void onClick(DialogInterface dialog, int which) {
        				return;
        		} });
        	dialog.show();
        	return;
		}
		
		// Validation was successful, save the settings object
		SharedPreferences settings = getSharedPreferences(
				Defaults.getSettingsName(), Defaults.getSettingsMode());
		SharedPreferences.Editor editor = settings.edit();
		
		editor.putString("username", username);
		editor.putString("password", password);
		editor.putInt("portNum", portNum);
		
		editor.commit();
		
		finish();  // close this Activity
	}
}
