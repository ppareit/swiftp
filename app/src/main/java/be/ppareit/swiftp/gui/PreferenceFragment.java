/*******************************************************************************
 * Copyright (c) 2012-2013 Pieter Pareit.
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * <p>
 * Contributors:
 * Pieter Pareit - initial API and implementation
 ******************************************************************************/

package be.ppareit.swiftp.gui;

import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.content.res.Resources;
import android.net.Uri;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiManager;
import android.os.Bundle;
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import android.text.util.Linkify;
import android.widget.BaseAdapter;
import android.widget.TextView;
import android.widget.Toast;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.net.InetAddress;
import java.util.List;

import be.ppareit.android.DynamicMultiSelectListPreference;
import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 */
public class PreferenceFragment extends android.preference.PreferenceFragment implements OnSharedPreferenceChangeListener {

    private EditTextPreference mPassWordPref;
    private Handler mHandler = new Handler();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);

        final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(getActivity());
        Resources resources = getResources();

        TwoStatePreference runningPref = findPref("running_switch");
        updateRunningState();
        runningPref.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((Boolean) newValue) {
                startServer();
            } else {
                stopServer();
            }
            return true;
        });

        PreferenceScreen prefScreen = findPref("preference_screen");
        Preference marketVersionPref = findPref("market_version");
        marketVersionPref.setOnPreferenceClickListener(preference -> {
            // start the market at our application
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setData(Uri.parse("market://details?id=be.ppareit.swiftp"));
            try {
                // this can fail if there is no market installed
                startActivity(intent);
            } catch (Exception e) {
                Cat.e("Failed to launch the market.");
                e.printStackTrace();
            }
            return false;
        });
        if (!App.isFreeVersion()) {
            prefScreen.removePreference(marketVersionPref);
        }

        updateLoginInfo();

        EditTextPreference usernamePref = findPref("username");
        usernamePref.setOnPreferenceChangeListener((preference, newValue) -> {
            String newUsername = (String) newValue;
            if (preference.getSummary().equals(newUsername))
                return false;
            if (!newUsername.matches("[a-zA-Z0-9]+")) {
                Toast.makeText(getActivity(),
                        R.string.username_validation_error, Toast.LENGTH_LONG).show();
                return false;
            }
            stopServer();
            return true;
        });

        mPassWordPref = findPref("password");
        mPassWordPref.setOnPreferenceChangeListener((preference, newValue) -> {
            stopServer();
            return true;
        });
        DynamicMultiSelectListPreference autoconnectListPref = findPref("autoconnect_preference");
        autoconnectListPref.setOnPopulateListener(
                pref -> {
                    Cat.d("autoconnect populate listener");

                    WifiManager wifiManager = (WifiManager)
                            getActivity().getApplicationContext().getSystemService(Context.WIFI_SERVICE);
                    List<WifiConfiguration> configs = wifiManager.getConfiguredNetworks();
                    if (configs == null) {
                        Cat.e("Unable to receive wifi configurations, bark at user and bail");
                        Toast.makeText(getActivity(),
                                R.string.autoconnect_error_enable_wifi_for_access_points,
                                Toast.LENGTH_LONG)
                                .show();
                        return;
                    }
                    CharSequence[] ssids = new CharSequence[configs.size()];
                    CharSequence[] niceSsids = new CharSequence[configs.size()];
                    for (int i = 0; i < configs.size(); ++i) {
                        ssids[i] = configs.get(i).SSID;
                        String ssid = configs.get(i).SSID;
                        if (ssid.length() > 2 && ssid.startsWith("\"") && ssid.endsWith("\"")) {
                            ssid = ssid.substring(1, ssid.length() - 1);
                        }
                        niceSsids[i] = ssid;
                    }
                    pref.setEntries(niceSsids);
                    pref.setEntryValues(ssids);
                });

        EditTextPreference portnum_pref = findPref("portNum");
        portnum_pref.setSummary(sp.getString("portNum",
                resources.getString(R.string.portnumber_default)));
        portnum_pref.setOnPreferenceChangeListener((preference, newValue) -> {
            String newPortnumString = (String) newValue;
            if (preference.getSummary().equals(newPortnumString))
                return false;
            int portnum = 0;
            try {
                portnum = Integer.parseInt(newPortnumString);
            } catch (Exception e) {
                Cat.d("Error parsing port number! Moving on...");
            }
            if (portnum <= 0 || 65535 < portnum) {
                Toast.makeText(getActivity(),
                        R.string.port_validation_error, Toast.LENGTH_LONG).show();
                return false;
            }
            preference.setSummary(newPortnumString);
            stopServer();
            return true;
        });

        Preference chroot_pref = findPref("chrootDir");
        chroot_pref.setSummary(FsSettings.getChrootDirAsString());
        chroot_pref.setOnPreferenceClickListener(preference -> {
            AlertDialog folderPicker = new FolderPickerDialogBuilder(getActivity(), FsSettings.getChrootDir())
                    .setSelectedButton(R.string.select, path -> {
                        if (preference.getSummary().equals(path))
                            return;
                        if (!FsSettings.setChrootDir(path))
                            return;
                        // TODO: this is a hotfix, create correct resources, improve UI/UX
                        final File root = new File(path);
                        if (!root.canRead()) {
                            Toast.makeText(getActivity(),
                                    "Notice that we can't read/write in this folder.",
                                    Toast.LENGTH_LONG).show();
                        } else if (!root.canWrite()) {
                            Toast.makeText(getActivity(),
                                    "Notice that we can't write in this folder, reading will work. Writing in subfolders might work.",
                                    Toast.LENGTH_LONG).show();
                        }

                        preference.setSummary(path);
                        stopServer();
                    })
                    .setNegativeButton(R.string.cancel, null)
                    .create();
            folderPicker.show();
            return true;
        });

        final CheckBoxPreference wakelock_pref = findPref("stayAwake");
        wakelock_pref.setOnPreferenceChangeListener((preference, newValue) -> {
            stopServer();
            return true;
        });

        ListPreference theme = findPref("theme");
        theme.setSummary(theme.getEntry());
        theme.setOnPreferenceChangeListener((preference, newValue) -> {
            theme.setSummary(theme.getEntry());
            getActivity().recreate();
            return true;
        });

        Preference help = findPref("help");
        help.setOnPreferenceClickListener(preference -> {
            Cat.v("On preference help clicked");
            Context context = getActivity();
            AlertDialog ad = new AlertDialog.Builder(context)
                    .setTitle(R.string.help_dlg_title)
                    .setMessage(R.string.help_dlg_message)
                    .setPositiveButton(android.R.string.ok, null)
                    .create();
            ad.show();
            Linkify.addLinks((TextView) ad.findViewById(android.R.id.message),
                    Linkify.ALL);
            return true;
        });

        Preference about = findPref("about");
        about.setOnPreferenceClickListener(preference -> {
            startActivity(new Intent(getActivity(), AboutActivity.class));
            return true;
        });

    }

    @Override
    public void onResume() {
        super.onResume();

        updateRunningState();

        Cat.d("onResume: Register the preference change listner");
        SharedPreferences sp = getPreferenceScreen().getSharedPreferences();
        sp.registerOnSharedPreferenceChangeListener(this);

        Cat.d("onResume: Registering the FTP server actions");
        IntentFilter filter = new IntentFilter();
        filter.addAction(FsService.ACTION_STARTED);
        filter.addAction(FsService.ACTION_STOPPED);
        filter.addAction(FsService.ACTION_FAILEDTOSTART);
        getActivity().registerReceiver(mFsActionsReceiver, filter);
    }

    @Override
    public void onPause() {
        super.onPause();

        Cat.v("onPause: Unregistering the FTPServer actions");
        getActivity().unregisterReceiver(mFsActionsReceiver);

        Cat.d("onPause: Unregistering the preference change listner");
        SharedPreferences sp = getPreferenceScreen().getSharedPreferences();
        sp.unregisterOnSharedPreferenceChangeListener(this);
    }

    @Override
    public void onSharedPreferenceChanged(SharedPreferences sp, String key) {
        updateLoginInfo();
    }

    private void startServer() {
        getActivity().sendBroadcast(new Intent(FsService.ACTION_START_FTPSERVER));
    }

    private void stopServer() {
        getActivity().sendBroadcast(new Intent(FsService.ACTION_STOP_FTPSERVER));
    }

    private void updateLoginInfo() {

        String username = FsSettings.getUserName();
        String password = FsSettings.getPassWord();

        Cat.v("Updating login summary");
        PreferenceScreen loginPreference = findPref("login");
        loginPreference.setSummary(username + " : " + transformPassword(password));
        ((BaseAdapter) loginPreference.getRootAdapter()).notifyDataSetChanged();

        EditTextPreference usernamePref = findPref("username");
        usernamePref.setSummary(username);

        EditTextPreference passWordPref = findPref("password");
        passWordPref.setSummary(transformPassword(password));
    }

    private void updateRunningState() {
        Resources res = getResources();
        TwoStatePreference runningPref = findPref("running_switch");
        if (FsService.isRunning()) {
            runningPref.setChecked(true);
            // Fill in the FTP server address
            InetAddress address = FsService.getLocalInetAddress();
            if (address == null) {
                Cat.v("Unable to retrieve wifi ip address");
                runningPref.setSummary(R.string.running_summary_failed_to_get_ip_address);
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
            Cat.v("action received: " + intent.getAction());
            // remove all pending callbacks
            mHandler.removeCallbacksAndMessages(null);
            // action will be ACTION_STARTED or ACTION_STOPPED
            updateRunningState();
            // or it might be ACTION_FAILEDTOSTART
            final TwoStatePreference runningPref = findPref("running_switch");
            if (intent.getAction().equals(FsService.ACTION_FAILEDTOSTART)) {
                runningPref.setChecked(false);
                mHandler.postDelayed(
                        () -> runningPref.setSummary(R.string.running_summary_failed),
                        100);
                mHandler.postDelayed(
                        () -> runningPref.setSummary(R.string.running_summary_stopped),
                        5000);
            }
        }
    };

    static private String transformPassword(String password) {
        Context context = App.getAppContext();
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        Resources res = context.getResources();
        String showPasswordString = res.getString(R.string.show_password_default);
        boolean showPassword = showPasswordString.equals("true");
        showPassword = sp.getBoolean("show_password", showPassword);
        if (showPassword)
            return password;
        else {
            StringBuilder sb = new StringBuilder(password.length());
            for (int i = 0; i < password.length(); ++i)
                sb.append('*');
            return sb.toString();
        }
    }

    @SuppressWarnings({"unchecked", "deprecation"})
    protected <T extends Preference> T findPref(CharSequence key) {
        return (T) findPreference(key);
    }

}
