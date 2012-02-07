package org.swiftp.gui;

import java.net.InetAddress;

import org.swiftp.FTPServerService;
import org.swiftp.Globals;
import org.swiftp.R;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Environment;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.Gravity;
import android.widget.Toast;

public class ServerPreferenceActivity extends PreferenceActivity {

    private static String TAG = ServerPreferenceActivity.class.getSimpleName();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);

        Globals.setContext(getApplicationContext());
        SharedPreferences settings = PreferenceManager
                .getDefaultSharedPreferences(this);

        CheckBoxPreference running_state = (CheckBoxPreference) findPreference("running_state");
        running_state.setChecked(FTPServerService.isRunning());
        running_state
                .setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
                    @Override
                    public boolean onPreferenceChange(Preference preference,
                            Object newValue) {
                        Context context = getApplicationContext();
                        Intent service = new Intent(context,
                                FTPServerService.class);
                        if ((Boolean) newValue) {
                            if (!FTPServerService.isRunning()) {
                                // Start the FTP server
                                warnIfNoExternalStorage();
                                context.startService(service);
                            }
                        } else {
                            context.stopService(service);
                        }
                        return true;
                    }
                });

        EditTextPreference username_pref = (EditTextPreference) findPreference("username");
        username_pref.setSummary(settings.getString("username", "ftp"));

        EditTextPreference password_pref = (EditTextPreference) findPreference("password");
        password_pref.setSummary(settings.getString("password", "ftp"));

        EditTextPreference portnum_pref = (EditTextPreference) findPreference("portNum");
        portnum_pref.setSummary(settings.getString("portNum", "2121"));
    }

    @Override
    protected void onResume() {
        Log.v(TAG, "onResume");
        super.onResume();

        Log.v(TAG, "Registering the FTP server actions");
        IntentFilter filter = new IntentFilter();
        filter.addAction(FTPServerService.ACTION_STARTED);
        filter.addAction(FTPServerService.ACTION_STOPPED);
        registerReceiver(ftpServerReceiver, filter);
    }

    @Override
    protected void onPause() {
        Log.v(TAG, "onPause");
        super.onPause();

        Log.v(TAG, "Unregistering the FTPServer actions");
        unregisterReceiver(ftpServerReceiver);
    }

    BroadcastReceiver ftpServerReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.v(TAG, "A FTPServer action received");
            CheckBoxPreference running_state = (CheckBoxPreference) findPreference("running_state");
            if (intent.getAction().equals(FTPServerService.ACTION_STARTED)) {
                running_state.setChecked(true);
                // Fill in the FTP server address
                InetAddress address = FTPServerService.getWifiIp();
                String iptext = "ftp://" + address.getHostAddress() + ":"
                        + FTPServerService.getPort() + "/";
                running_state.setSummary(iptext);
            } else if (intent.getAction().equals(FTPServerService.ACTION_STOPPED)){
                running_state.setChecked(false);
                running_state.setSummary(R.string.running_summary);
            }
        }
    };

    private void warnIfNoExternalStorage() {
        String storageState = Environment.getExternalStorageState();
        if (!storageState.equals(Environment.MEDIA_MOUNTED)) {
            Log.v(TAG, "Warning due to storage state " + storageState);
            Toast toast = Toast.makeText(this, R.string.storage_warning,
                    Toast.LENGTH_LONG);
            toast.setGravity(Gravity.CENTER, 0, 0);
            toast.show();
        }
    }

}
