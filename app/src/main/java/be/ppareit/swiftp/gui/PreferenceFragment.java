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

import android.Manifest;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.net.Uri;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import android.text.util.Linkify;
import android.widget.TextView;
import android.widget.Toast;

import net.vrallev.android.cat.Cat;

import java.net.InetAddress;
import java.util.List;
import java.util.Set;

import be.ppareit.android.DynamicMultiSelectListPreference;
import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import lombok.val;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 */
public class PreferenceFragment extends android.preference.PreferenceFragment {

    private static final int ACCESS_COARSE_LOCATION_REQUEST_CODE = 14;
    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;

    private DynamicMultiSelectListPreference mAutoconnectListPref;
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
                FsService.start();
            } else {
                FsService.stop();
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

        Preference manageUsersPref = findPref("manage_users");
        updateUsersPref();
        manageUsersPref.setOnPreferenceClickListener((preference) -> {
            startActivity(new Intent(getActivity(), ManageUsersActivity.class));
            return true;
        });

        mAutoconnectListPref = findPref("autoconnect_preference");
        mAutoconnectListPref.setOnPopulateListener(
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
                    pref.setValues(FsSettings.getAutoConnectList());
                });
        mAutoconnectListPref.setOnPreferenceClickListener(preference -> {
            Cat.d("Clicked to open auto connect list preference");
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                if (ActivityCompat.checkSelfPermission(getContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_DENIED) {
                    if (ActivityCompat.shouldShowRequestPermissionRationale(getActivity(), Manifest.permission.ACCESS_COARSE_LOCATION)) {
                        new AlertDialog.Builder(getContext())
                                .setTitle(R.string.request_coarse_location_dlg_title)
                                .setMessage(R.string.request_coarse_location_dlg_message)
                                .setPositiveButton(android.R.string.ok, (dialog, which) -> {
                                    requestPermissions(new String[]{Manifest.permission.ACCESS_COARSE_LOCATION}, ACCESS_COARSE_LOCATION_REQUEST_CODE);
                                })
                                .setOnCancelListener(dialog -> {
                                    mAutoconnectListPref.getDialog().cancel();
                                })
                                .create()
                                .show();
                    } else {
                        requestPermissions(new String[]{Manifest.permission.ACCESS_COARSE_LOCATION}, ACCESS_COARSE_LOCATION_REQUEST_CODE);
                    }
                }
            }
            return false;
        });
        mAutoconnectListPref.setOnPreferenceChangeListener((preference, newValue) -> {
            Cat.d("Changed auto connect list preference");

            Set<String> oldList = FsSettings.getAutoConnectList();
            Set<String> newList = (Set<String>) newValue;

            Cat.d("Old List: " + oldList + " New List: " + newList);

            WifiManager wifiManager = (WifiManager) getActivity().getApplicationContext()
                    .getSystemService(Context.WIFI_SERVICE);
            if (wifiManager == null) {
                return true;
            }
            WifiInfo wifiInfo = wifiManager.getConnectionInfo();
            if (wifiInfo == null) {
                Cat.e("Null wifi info received, bailing");
                return true;
            }
            Cat.d("We are connected to " + wifiInfo.getSSID());
            if (newList.contains(wifiInfo.getSSID())) {
                FsService.start();
            }
            if (oldList.contains(wifiInfo.getSSID()) && !newList.contains(wifiInfo.getSSID())) {
                FsService.stop();
            }
            return true;
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
            FsService.stop();
            return true;
        });

        final CheckBoxPreference wakelock_pref = findPref("stayAwake");
        wakelock_pref.setOnPreferenceChangeListener((preference, newValue) -> {
            FsService.stop();
            return true;
        });

        final CheckBoxPreference writeExternalStorage_pref = findPref("writeExternalStorage");
        String externalStorageUri = FsSettings.getExternalStorageUri();
        if (externalStorageUri == null) {
            writeExternalStorage_pref.setChecked(false);
        }
        writeExternalStorage_pref.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
                startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
                return false;
            } else {
                FsSettings.setExternalStorageUri(null);
                return true;
            }
        });


        ListPreference theme = findPref("theme");
        theme.setSummary(theme.getEntry());
        theme.setOnPreferenceChangeListener((preference, newValue) -> {
            theme.setSummary(theme.getEntry());
            getActivity().recreate();
            return true;
        });

        val showNotificationIconPref = findPref("show_notification_icon_preference");
        showNotificationIconPref.setOnPreferenceChangeListener((preference, newValue) -> {
            getActivity().sendBroadcast(new Intent(FsNotification.ACTION_UPDATE_NOTIFICATION));
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
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        if (requestCode == ACCESS_COARSE_LOCATION_REQUEST_CODE) {
            if (permissions[0].equals(Manifest.permission.ACCESS_COARSE_LOCATION) && grantResults[0] == PackageManager.PERMISSION_DENIED) {
                mAutoconnectListPref.getDialog().cancel();
            }
        }
    }

    @Override
    public void onResume() {
        super.onResume();

        updateRunningState();
        updateUsersPref();

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
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent resultData) {
        Cat.d("onActivityResult called");
        if (requestCode == ACTION_OPEN_DOCUMENT_TREE && resultCode == Activity.RESULT_OK) {
            Uri treeUri = resultData.getData();
            String path = treeUri.getPath();

            final CheckBoxPreference writeExternalStorage_pref = findPref("writeExternalStorage");
            if (!":".equals(path.substring(path.length() - 1)) || path.contains("primary")) {
                writeExternalStorage_pref.setChecked(false);
            } else {
                FsSettings.setExternalStorageUri(treeUri.toString());
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                    getActivity().getContentResolver().takePersistableUriPermission(treeUri,
                            Intent.FLAG_GRANT_READ_URI_PERMISSION |
                                    Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
                }
                writeExternalStorage_pref.setChecked(true);
            }
        }

    }

    private void updateUsersPref() {
        val manageUsersPref = findPref("manage_users");
        val users = FsSettings.getUsers();
        switch (users.size()) {
            case 0:
                manageUsersPref.setSummary(R.string.manage_users_no_users);
                break;
            case 1:
                val user = users.get(0);
                manageUsersPref.setSummary(user.getUsername() + ":" + user.getPassword());
                break;
            default:
                manageUsersPref.setSummary(R.string.manage_users_multiple_users);
        }
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

    @SuppressWarnings({"unchecked", "deprecation"})
    protected <T extends Preference> T findPref(CharSequence key) {
        return (T) findPreference(key);
    }

}
