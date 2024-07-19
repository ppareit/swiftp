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
import android.content.UriPermission;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import android.provider.Settings;
import android.text.util.Linkify;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.core.app.ActivityCompat;
import androidx.documentfile.provider.DocumentFile;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.net.InetAddress;
import java.util.List;

import be.ppareit.android.DynamicMultiSelectListPreference;
import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.FileUtil;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 */
public class PreferenceFragment extends android.preference.PreferenceFragment {

    private static final int STORAGE_PERMISSION_CODE = 100;

    private Handler mHandler = new Handler();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);

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
        if (!App.isFreeVersion()) {
            prefScreen.removePreference(marketVersionPref);
        }
        if (!(App.isPackageInstalled("com.android.vending") ||
                App.isPackageInstalled("com.google.market"))) {
            prefScreen.removePreference(marketVersionPref);
        }
        marketVersionPref.setOnPreferenceClickListener(preference -> {
            // start the market at our application
            Intent intent = new Intent(Intent.ACTION_VIEW);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            intent.setData(Uri.parse("market://details?id=be.ppareit.swiftp"));
            startActivity(intent);
            return false;
        });

        Preference manageUsersPref = findPref("manage_users");
        updateUsersPref();
        manageUsersPref.setOnPreferenceClickListener((preference) -> {
            startActivity(new Intent(getActivity(), ManageUsersActivity.class));
            return true;
        });

        EditTextPreference portNumberPref = findPref("portNum");
        portNumberPref.setSummary(String.valueOf(FsSettings.getPortNumber()));
        portNumberPref.setOnPreferenceChangeListener((preference, newValue) -> {
            String newPortNumberString = (String) newValue;
            if (preference.getSummary().equals(newPortNumberString))
                return false;
            int portNumber = 0;
            try {
                portNumber = Integer.parseInt(newPortNumberString);
            } catch (Exception e) {
                Cat.d("Error parsing port number! Moving on...");
            }
            if (portNumber <= 0 || 65535 < portNumber) {
                Toast.makeText(getActivity(),
                        R.string.port_validation_error,
                        Toast.LENGTH_LONG).show();
                return false;
            }
            preference.setSummary(newPortNumberString);
            FsService.stop();
            return true;
        });

        final CheckBoxPreference wakelockPref = findPref("stayAwake");
        wakelockPref.setOnPreferenceChangeListener((preference, newValue) -> {
            FsService.stop();
            return true;
        });

        final CheckBoxPreference writeExternalStoragePref = findPref("writeExternalStorage");
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.LOLLIPOP) {
            String externalStorageUri = FsSettings.getExternalStorageUri();
            if (externalStorageUri == null) {
                writeExternalStoragePref.setChecked(false);
            }
            writeExternalStoragePref.setOnPreferenceChangeListener((preference, newValue) -> {
                if ((boolean) newValue) {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                        //Android is 11(R) or above
                        Intent intent = new Intent(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION);
                        Uri uri = Uri.fromParts("package", this.getActivity().getPackageName(), null);
                        intent.setData(uri);
                        startActivityForResult(intent, STORAGE_PERMISSION_CODE);
                    } else {
                        //Android is below 11(R)
                        String[] permissions = new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE};
                        requestPermissions(permissions, STORAGE_PERMISSION_CODE);
                    }

                    return false;
                } else {
                    FsSettings.setExternalStorageUri(null);
                    return true;
                }
            });
        } else {
            writeExternalStoragePref.setEnabled(false);
            writeExternalStoragePref.setChecked(true);
            writeExternalStoragePref.setSummary(getString(R.string.write_external_storage_old_android_version_summary));
        }

        final ListPreference batterySaver = findPref("battery_saver");
        // val 0 HIGH is always on wake locks + wake lock setting enabled (high battery, smooth)
        // val 1 LOW is wake locks run only during client connection (low battery, some of both)
        // val 2 DEEP is wake locks disabled (lowest battery use, a bit choppy)
        final String s = batterySaver.getValue();
        if (Integer.parseInt(s) > 1) {
            wakelockPref.setChecked(false);
            wakelockPref.setEnabled(false);
        } else {
            wakelockPref.setEnabled(true);
        }
        batterySaver.setTitle("Battery saver");
        final String bSumSelection = FsSettings.getBatterySaverChoice(null) + '\n';
        final String bSum = bSumSelection + getString(R.string.battery_saver_desc);
        batterySaver.setSummary(bSum);
        batterySaver.setOnPreferenceChangeListener((preference, newValue) -> {
            if (Integer.parseInt((String) newValue) > 1) {
                wakelockPref.setChecked(false);
                wakelockPref.setEnabled(false);
            } else {
                wakelockPref.setEnabled(true);
            }
            final String bSumSelection2 = FsSettings.getBatterySaverChoice(
                    (String) newValue) + '\n';
            final String bSum2 = bSumSelection2 + getString(R.string.battery_saver_desc);
            batterySaver.setSummary(bSum2);
            return true;
        });

        ListPreference themePref = findPref("theme");
        themePref.setSummary(themePref.getEntry());
        themePref.setOnPreferenceChangeListener((preference, newValue) -> {
            themePref.setSummary(themePref.getEntry());
            getActivity().recreate();
            return true;
        });

        Preference showNotificationIconPref = findPref("show_notification_icon_preference");
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            PreferenceScreen appearanceScreen = (PreferenceScreen) findPreference("appearance_screen");
            appearanceScreen.removePreference(showNotificationIconPref);
        }
        showNotificationIconPref.setOnPreferenceChangeListener((preference, newValue) -> {
            FsService.stop();
            return true;
        });

        Preference helpPref = findPref("help");
        helpPref.setOnPreferenceClickListener(preference -> {
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

        Preference aboutPref = findPref("about");
        aboutPref.setOnPreferenceClickListener(preference -> {
            startActivity(new Intent(getActivity(), AboutActivity.class));
            return true;
        });

    }


    public void onRequestPermissionsResult(int requestCode,
                                           @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == STORAGE_PERMISSION_CODE) {
            if (grantResults.length >= 2) {
                //check each permission if granted or not
                boolean write = grantResults[0] == PackageManager.PERMISSION_GRANTED;
                boolean read = grantResults[1] == PackageManager.PERMISSION_GRANTED;
                if (write && read) {
                    //External Storage Permission granted
                    final CheckBoxPreference writeExternalStoragePref = findPref("writeExternalStorage");
                    writeExternalStoragePref.setEnabled(false);
                    writeExternalStoragePref.setChecked(true);
                } else {
                    //External Storage Permission denied...
                    Toast.makeText(this.getContext(), "Something went wrong !", Toast.LENGTH_LONG).show();
                }
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
        if (Build.VERSION.SDK_INT >= 33) {
            getActivity().registerReceiver(mFsActionsReceiver, filter, FsService.RECEIVER_EXPORTED);
        } else {
            getActivity().registerReceiver(mFsActionsReceiver, filter);
        }
    }

    @Override
    public void onPause() {
        super.onPause();

        Cat.v("onPause: Unregistering the FTPServer actions");
        getActivity().unregisterReceiver(mFsActionsReceiver);
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent resultData) {

        //here we will handle the result of our intent
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R){
            //Android is 11(R) or above
            if (Environment.isExternalStorageManager()){
                //Manage External Storage Permission is granted
                final CheckBoxPreference writeExternalStoragePref = findPref("writeExternalStorage");
                writeExternalStoragePref.setEnabled(false);
                writeExternalStoragePref.setChecked(true);
            }
            else{
                //Manage External Storage Permission is denied....
                Toast.makeText(this.getContext(), "Something went wrong !", Toast.LENGTH_LONG).show();
            }
        }
        else{
            //Android is below 11(R)
        }
    }


    /**
     * Update the summary for the users. When there are no users, ask to add at least one user.
     * When there is one user, display helpful message about user/password. When there are
     * multiple users, refer to the list.
     */
    private void updateUsersPref() {
        Preference manageUsersPref = findPref("manage_users");
        List<FtpUser> users = FsSettings.getUsers();
        switch (users.size()) {
            case 0:
                manageUsersPref.setSummary(R.string.manage_users_no_users);
                break;
            case 1:
                FtpUser user = users.get(0);
                manageUsersPref.setSummary(user.getUsername() + ":" + user.getPassword());
                break;
            default:
                manageUsersPref.setSummary(R.string.manage_users_multiple_users);
        }
    }

    /**
     * Display helpful message in the summary about the state of the ftp server. When the
     * server is running, display the ip address to reach it. When there was
     * a failure starting the server, let this know.
     */
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
            String ipText = "ftp://" + address.getHostAddress() + ":"
                    + FsSettings.getPortNumber() + "/";
            String summary = res.getString(R.string.running_summary_started, ipText);
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
            if (intent.getAction() == null) {
                return;
            }
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

    @SuppressWarnings({"unchecked"})
    protected <T extends Preference> T findPref(CharSequence key) {
        return (T) findPreference(key);
    }

}
