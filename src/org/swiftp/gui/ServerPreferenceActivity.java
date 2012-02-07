package org.swiftp.gui;

import org.swiftp.FTPServerService;
import org.swiftp.Globals;
import org.swiftp.R;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.PreferenceActivity;
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

        CheckBoxPreference running_state = (CheckBoxPreference)findPreference("running_state");
        running_state.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                Context context = getApplicationContext();
                Intent service = new Intent(context, FTPServerService.class);
                if ((Boolean)newValue) {
                    if (!FTPServerService.isRunning()) {
                        warnIfNoExternalStorage();
                        context.startService(service);
                    }
                } else {
                    context.stopService(service);
                }
                return true;
            }
        });
    }

    private void warnIfNoExternalStorage() {
        String storageState = Environment.getExternalStorageState();
        if(!storageState.equals(Environment.MEDIA_MOUNTED)) {
            Log.v(TAG, "Warning due to storage state " + storageState);
            Toast toast = Toast.makeText(this, R.string.storage_warning,
                    Toast.LENGTH_LONG);
            toast.setGravity(Gravity.CENTER, 0, 0);
            toast.show();
        }
    }

}
