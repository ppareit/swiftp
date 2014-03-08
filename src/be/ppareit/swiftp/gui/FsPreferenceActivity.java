/*******************************************************************************
 * Copyright (c) 2012-2013 Pieter Pareit.
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Contributors:
 *     Pieter Pareit - initial API and implementation
 ******************************************************************************/

package be.ppareit.swiftp.gui;

import java.io.File;
import java.net.InetAddress;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.content.res.Resources;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import android.text.util.Linkify;
import android.util.Log;
import android.widget.TextView;
import android.widget.Toast;
import be.ppareit.swiftp.FsApp;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.FsSettings;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 * 
 */
public class FsPreferenceActivity extends PreferenceActivity implements
        OnSharedPreferenceChangeListener {

    private static String TAG = FsPreferenceActivity.class.getSimpleName();

    private EditTextPreference mPassWordPref;
    private Handler mHandler = new Handler();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d(TAG, "created");
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);

        final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        Resources resources = getResources();

        TwoStatePreference runningPref = findPref("running_switch");
        updateRunningState();
        runningPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                if ((Boolean) newValue) {
                    startServer();
                } else {
                    stopServer();
                }
                return true;
            }
        });

        PreferenceScreen prefScreen = findPref("preference_screen");
        Preference marketVersionPref = findPref("market_version");
        marketVersionPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {
                // start the market at our application
                Intent intent = new Intent(Intent.ACTION_VIEW);
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                intent.setData(Uri.parse("market://details?id=be.ppareit.swiftp"));
                try {
                    // this can fail if there is no market installed
                    startActivity(intent);
                } catch (Exception e) {
                    Log.e(TAG, "Failed to lauch the market.");
                    e.printStackTrace();
                }
                return false;
            }
        });
        if (FsApp.isFreeVersion() == false) {
            prefScreen.removePreference(marketVersionPref);
        }

        EditTextPreference username_pref = findPref("username");
        username_pref.setSummary(sp.getString("username",
                resources.getString(R.string.username_default)));
        username_pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                String newUsername = (String) newValue;
                if (preference.getSummary().equals(newUsername))
                    return false;
                if (!newUsername.matches("[a-zA-Z0-9]+")) {
                    Toast.makeText(FsPreferenceActivity.this,
                            R.string.username_validation_error, Toast.LENGTH_LONG).show();
                    return false;
                }
                preference.setSummary(newUsername);
                stopServer();
                return true;
            }
        });

        mPassWordPref = findPref("password");
        String password = resources.getString(R.string.password_default);
        password = sp.getString("password", password);
        mPassWordPref.setSummary(transformPassword(password));
        mPassWordPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                String newPassword = (String) newValue;
                preference.setSummary(transformPassword(newPassword));
                stopServer();
                return true;
            }
        });

        EditTextPreference portnum_pref = findPref("portNum");
        portnum_pref.setSummary(sp.getString("portNum",
                resources.getString(R.string.portnumber_default)));
        portnum_pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                String newPortnumString = (String) newValue;
                if (preference.getSummary().equals(newPortnumString))
                    return false;
                int portnum = 0;
                try {
                    portnum = Integer.parseInt(newPortnumString);
                } catch (Exception e) {
                }
                if (portnum <= 0 || 65535 < portnum) {
                    Toast.makeText(FsPreferenceActivity.this,
                            R.string.port_validation_error, Toast.LENGTH_LONG).show();
                    return false;
                }
                preference.setSummary(newPortnumString);
                stopServer();
                return true;
            }
        });

        EditTextPreference chroot_pref = findPref("chrootDir");
        chroot_pref.setSummary(FsSettings.getChrootDir().getAbsolutePath());
        chroot_pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                String newChroot = (String) newValue;
                if (preference.getSummary().equals(newChroot))
                    return false;
                // now test the new chroot directory
                File chrootTest = new File(newChroot);
                if (!chrootTest.isDirectory() || !chrootTest.canRead())
                    return false;
                preference.setSummary(newChroot);
                stopServer();
                return true;
            }
        });

        final CheckBoxPreference wakelock_pref = findPref("stayAwake");
        wakelock_pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                stopServer();
                return true;
            }
        });

        Preference help = findPref("help");
        help.setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {
                Log.v(TAG, "On preference help clicked");
                Context context = FsPreferenceActivity.this;
                AlertDialog ad = new AlertDialog.Builder(context)
                        .setTitle(R.string.help_dlg_title)
                        .setMessage(R.string.help_dlg_message)
                        .setPositiveButton(R.string.ok, null).create();
                ad.show();
                Linkify.addLinks((TextView) ad.findViewById(android.R.id.message),
                        Linkify.ALL);
                return true;
            }
        });

        Preference about = findPref("about");
        about.setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {
                AlertDialog ad = new AlertDialog.Builder(FsPreferenceActivity.this)
                        .setTitle(R.string.about_dlg_title)
                        .setMessage(R.string.about_dlg_message)
                        .setPositiveButton(getText(R.string.ok), null).create();
                ad.show();
                Linkify.addLinks((TextView) ad.findViewById(android.R.id.message),
                        Linkify.ALL);
                return true;
            }
        });

    }

    @Override
    protected void onResume() {
        super.onResume();

        updateRunningState();

        Log.d(TAG, "onResume: Register the preference change listner");
        SharedPreferences sp = getPreferenceScreen().getSharedPreferences();
        sp.registerOnSharedPreferenceChangeListener(this);

        Log.d(TAG, "onResume: Registering the FTP server actions");
        IntentFilter filter = new IntentFilter();
        filter.addAction(FsService.ACTION_STARTED);
        filter.addAction(FsService.ACTION_STOPPED);
        filter.addAction(FsService.ACTION_FAILEDTOSTART);
        registerReceiver(mFsActionsReceiver, filter);
    }

    @Override
    protected void onPause() {
        super.onPause();

        Log.v(TAG, "onPause: Unregistering the FTPServer actions");
        unregisterReceiver(mFsActionsReceiver);

        Log.d(TAG, "onPause: Unregistering the preference change listner");
        SharedPreferences sp = getPreferenceScreen().getSharedPreferences();
        sp.unregisterOnSharedPreferenceChangeListener(this);
    }

    @Override
    public void onSharedPreferenceChanged(SharedPreferences sp, String key) {
        if (key.equals("show_password")) {
            Context context = FsApp.getAppContext();
            Resources res = context.getResources();
            String password = res.getString(R.string.password_default);
            password = sp.getString("password", password);
            mPassWordPref.setSummary(transformPassword(password));
        }
    }

    private void startServer() {
        sendBroadcast(new Intent(FsService.ACTION_START_FTPSERVER));
    }

    private void stopServer() {
        sendBroadcast(new Intent(FsService.ACTION_STOP_FTPSERVER));
    }

    private void updateRunningState() {
        Resources res = getResources();
        TwoStatePreference runningPref = findPref("running_switch");
        if (FsService.isRunning() == true) {
            runningPref.setChecked(true);
            // Fill in the FTP server address
            InetAddress address = FsService.getLocalInetAddress();
            if (address == null) {
                Log.v(TAG, "Unable to retreive wifi ip address");
                runningPref.setSummary(R.string.cant_get_url);
                return;
            }
            String iptext = "ftp://" + address.getHostAddress() + ":"
                    + FsSettings.getPortNumber() + "/";
            String summary = res.getString(R.string.running_summary_started, iptext);
            runningPref.setSummary(summary);
        } else {
            runningPref.setChecked(false);
            runningPref.setSummary(R.string.running_summary_stopped);
        }
    }

    /**
     * This receiver will check FTPServer.ACTION* messages and will update the button,
     * running_state, if the server is running and will also display at what url the
     * server is running.
     */
    BroadcastReceiver mFsActionsReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.v(TAG, "FTPServerService action received: " + intent.getAction());
            // action will be ACTION_STARTED or ACTION_STOPPED
            updateRunningState();
            // or it might be ACTION_FAILEDTOSTART
            final TwoStatePreference runningPref = findPref("running_switch");
            if (intent.getAction().equals(FsService.ACTION_FAILEDTOSTART)) {
                runningPref.setChecked(false);
                mHandler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        runningPref.setSummary(R.string.running_summary_failed);
                    }
                }, 100);
                mHandler.postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        runningPref.setSummary(R.string.running_summary_stopped);
                    }
                }, 5000);
            }
        }
    };

    static private String transformPassword(String password) {
        Context context = FsApp.getAppContext();
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        Resources res = context.getResources();
        String showPasswordString = res.getString(R.string.show_password_default);
        boolean showPassword = showPasswordString.equals("true");
        showPassword = sp.getBoolean("show_password", showPassword);
        if (showPassword == true)
            return password;
        else {
            StringBuilder sb = new StringBuilder(password.length());
            for (int i = 0; i < password.length(); ++i)
                sb.append('*');
            return sb.toString();
        }
    }

    @SuppressWarnings({ "unchecked", "deprecation" })
    protected <T extends Preference> T findPref(CharSequence key) {
        return (T) this.findPreference(key);
    }

}
