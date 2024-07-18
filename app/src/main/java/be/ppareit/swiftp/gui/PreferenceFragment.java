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
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import android.text.util.Linkify;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
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

    private static final int ACCESS_COARSE_LOCATION_REQUEST_CODE = 14;
    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;

    private DynamicMultiSelectListPreference mAutoconnectListPref;
    private Handler mHandler = new Handler();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.preferences);

        TwoStatePreference runningPref = findPref("running_switch");
        updateRunningState();
        runningPref.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((Boolean) newValue) {
                if (Util.useScopedStorage() && FsSettings.getExternalStorageUri() == null) {
                    // Seems like we are on a system that requires scoped storage,
                    // but scoped storage has not yet been configured, force it now
                    Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
                    startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
                }
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
                    Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
                    startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
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

    @RequiresApi(api = Build.VERSION_CODES.M)
    private void requestAccessCoarseLocationPermission() {
        String[] permissions = new String[]{Manifest.permission.ACCESS_COARSE_LOCATION};
        requestPermissions(permissions, ACCESS_COARSE_LOCATION_REQUEST_CODE);
    }

    @Override
    public void onRequestPermissionsResult(int requestCode,
                                           @NonNull String[] permissions,
                                           @NonNull int[] grantResults) {
        if (requestCode == ACCESS_COARSE_LOCATION_REQUEST_CODE) {
            if (permissions[0].equals(Manifest.permission.ACCESS_COARSE_LOCATION)
                    && grantResults[0] == PackageManager.PERMISSION_DENIED) {
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
        Cat.d("onActivityResult called");
        if (requestCode == ACTION_OPEN_DOCUMENT_TREE && resultCode == Activity.RESULT_OK) {
            if (resultData == null) return;
            Uri treeUri = resultData.getData();
            if (treeUri == null) return;
            String path = treeUri.getPath();
            Cat.d("Action Open Document Tree on path " + path);
            // *************************************
            // The order following here is critical. They must stay ordered as they are.
            setPermissionToUseExternalStorage(treeUri);
            tryToUpgradeToScopedStorage(treeUri);
            scopedStorageChrootOverride(treeUri);
        }
    }

    private void setPermissionToUseExternalStorage(Uri treeUri) {
        final CheckBoxPreference writeExternalStoragePref = findPref("writeExternalStorage");
        if (isNotExternalStorage(treeUri)) {
            writeExternalStoragePref.setChecked(false);
        } else {
            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                    if (removeAllUriPermissions(treeUri)) {
                        FsSettings.setExternalStorageUri(treeUri.toString());
                        getActivity().getContentResolver()
                                .takePersistableUriPermission(treeUri,
                                        Intent.FLAG_GRANT_READ_URI_PERMISSION
                                                | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
                    }
                }
                writeExternalStoragePref.setChecked(true);
            } catch (SecurityException e) {
                // Harden code against crash: May reach here by adding exact same picker location but
                // being removed at same time.
            }
        }
    }

    private boolean isNotExternalStorage(Uri treeUri) {
        String folder = FileUtil.cleanupUriStoragePath(treeUri);
        if (folder != null && folder.contains(":")) {
            // Just get rid of the "primary:" part to get what we want (the user selected path/folder)
            try {
                folder = folder.substring(folder.indexOf(":") + 1);
            } catch (IndexOutOfBoundsException e) {
                folder = "";
            }
        }
        return folder == null || folder.isEmpty();
    }

    /*
     * If user is on older SDK, check if File can rw and if not then move to newer storage use.
     * As we don't know what older SDK will have a problem where or not.
     * Could just assume with this use but a check is fast.
     * */
    private void tryToUpgradeToScopedStorage(Uri treeUri) {
        if (!Util.useScopedStorage()) {
            DocumentFile df = FileUtil.getDocumentFileFromUri(treeUri);
            if (df == null) return;
            final String a11Path = FileUtil.getUriStoragePathFullFromDocumentFile(df, "");
            if (a11Path == null) return;
            File root = new File(a11Path);
            if (!root.canRead() || !root.canWrite()) {
                SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
                // Fix: Use commit; override in next method using useScopedStorage() needs it.
                sp.edit().putBoolean("OverrideScopedStorageMinimum", true).commit();
            }
        }
    }

    /*
     * Override all and put path as chroot. Previously user chosen. On Android 11+ it is only
     * decided now by the picker if its to be allowed on the Play Store unless allowance was made.
     * */
    private void scopedStorageChrootOverride(Uri treeUri) {
        if (Util.useScopedStorage()) {
            DocumentFile df = FileUtil.getDocumentFileFromUri(treeUri);
            if (df == null) return;
            final String scopedStoragePath = FileUtil.getUriStoragePathFullFromDocumentFile(df, "");
            if (scopedStoragePath == null) return;
            List<FtpUser> userList = FsSettings.getUsers();
            for (int i = 0; i < userList.size(); i++) {
                if (userList.get(i) == null) continue;
                FtpUser entry = new FtpUser(userList.get(i).getUsername(), userList.get(i).getPassword(), scopedStoragePath);
                FsSettings.modifyUser(userList.get(i).getUsername(), entry);
            }
        }
    }

    /*
     * Clean up URI list since there's only one folder. They have a way of collecting on changes
     * which causes an issue. More so only can use one.
     * */
    private boolean removeAllUriPermissions(Uri treeUri) {
        List<UriPermission> oldList = App.getAppContext().getContentResolver().getPersistedUriPermissions();
        if (oldList.size() == 0) return true;
        // check against current and don't remove if only and same as it won't re-give same.
        if (oldList.size() == 1) {
            Uri uri = oldList.get(0).getUri();
            if (uri != null) {
                final String path = uri.getPath();
                if (path != null) if (path.equals(treeUri.getPath())) return false;
            }
        }
        // Release all
        for (UriPermission uriToRemove : oldList) {
            if (uriToRemove == null) continue;
            getActivity().getContentResolver()
                    .releasePersistableUriPermission(uriToRemove.getUri(),
                            Intent.FLAG_GRANT_READ_URI_PERMISSION
                                    | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
        }
        return true;
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
