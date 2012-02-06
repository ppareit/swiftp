package org.swiftp.gui;

import org.swiftp.FTPServerService;
import org.swiftp.Globals;
import org.swiftp.R;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.PreferenceActivity;

public class ServerPreferenceActivity extends PreferenceActivity {

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
                    context.startService(service);
                } else {
                    context.stopService(service);
                }
                return true;
            }
        });
    }

}
