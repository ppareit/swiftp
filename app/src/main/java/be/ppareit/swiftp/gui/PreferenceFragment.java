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
import android.preference.MultiSelectListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.PreferenceScreen;
import android.preference.TwoStatePreference;
import android.text.InputType;
import android.text.util.Linkify;
import android.util.ArraySet;
import android.view.inputmethod.EditorInfo;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;

import net.vrallev.android.cat.Cat;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import be.ppareit.android.DynamicMultiSelectListPreference;
import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.FTPSSockets;

import be.ppareit.swiftp.utils.Logging;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 */
public class PreferenceFragment extends android.preference.PreferenceFragment {

    private static final int ACCESS_COARSE_LOCATION_REQUEST_CODE = 14;
    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;
    private static final int PICK_CERT_FILE_JKS = 84;
    private static final int PICK_CERT_FILE_BKS = 85;

    private static final SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());

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

        Preference manageAnonPref = findPref("manage_anon");
        manageAnonPref.setOnPreferenceClickListener((preference) -> {
            startActivity(new Intent(getActivity(), ManageAnonActivity.class));
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
            FsService.restart();
            return true;
        });

        final CheckBoxPreference wakelockPref = findPref("stayAwake");
        wakelockPref.setOnPreferenceChangeListener((preference, newValue) -> {
            FsService.restart();
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
        final String bSum = bSumSelection;
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
            final String bSum2 = bSumSelection2;
            batterySaver.setSummary(bSum2);
            FsService.restart();
            return true;
        });

        final CheckBoxPreference useScopedStorage = findPref("useScopedStorage");
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
        if (sp.getBoolean("NewScopedStorageUpgradeCheck", true)) {
            // Don't break use if "write external storage" was used before the app update as the original
            // code it now fully uses isn't compat with the newer one and would see major issues.
            // Runs one time only on update as pref won't be checked after clean install / wipe.
            // Code is executed on app start which happens automatically after app update.
            if (writeExternalStoragePref.isChecked()) { // needs to be true to not break use
                sp.edit().putBoolean("UseScopedStorage", true).apply();
                sp.edit().putBoolean("NewScopedStorageUpgradeCheck", false).apply();
                writeExtMultiUserUpgradePath();
            }
        }

        if (Util.useScopedStorage()) {
            // Do not allow mixing of old setting with the new one!
            useScopedStorage.setChecked(true);
            writeExternalStoragePref.setChecked(false);
            writeExternalStoragePref.setEnabled(false);
        } else {
            useScopedStorage.setChecked(false);
        }
        useScopedStorage.setOnPreferenceChangeListener((preference, newValue) -> {
            writeExternalStoragePref.setChecked(false);
            writeExternalStoragePref.setEnabled(!((boolean) newValue));
            sp.edit().putBoolean("UseScopedStorage", (boolean) newValue).apply();
            Util.resetScoped();

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

        Preference logsPref = findPref("logs");
        logsPref.setOnPreferenceClickListener(preference -> {
            startActivity(new Intent(getActivity(), LogActivity.class));
            return true;
        });

        Preference logCheckbox = findPref("enable_logging");
        logCheckbox.setOnPreferenceChangeListener((preference, newValue) -> {
            if (!(boolean) newValue) new Logging().clearLog();
            return true;
        });

        // Used to set the low side of the port range for data connections.
        // Set both low and high to empty or 0 to use random.
        EditTextPreference pasvRangeLow = findPref("portRangePasvLow");
        pasvRangeLow.setOnPreferenceChangeListener((preference, newValue) -> {
            String lowDefault = getContext().getString(R.string.portnumber_default_pasv_low);
            preference.setSummary(checkNewPortValue((String) newValue, lowDefault));
            return true;
        });
        pasvRangeLow.setSummary(FsSettings.getPortRangeLowString());

        // Used to set the high side of the port range for data connections.
        // Set both low and high to empty or 0 to use random.
        EditTextPreference pasvRangeHigh = findPref("portRangePasvHigh");
        pasvRangeHigh.setOnPreferenceChangeListener((preference, newValue) -> {
            String highDefault = getContext().getString(R.string.portnumber_default_pasv_high);
            preference.setSummary(checkNewPortValue((String) newValue, highDefault));
            return true;
        });
        pasvRangeHigh.setSummary(FsSettings.getPortRangeHighString());

        // Allows user to choos the TLS implicit port
        EditTextPreference impicitPort = findPref("portNumImplicit");
        String implicitPortDefault = getContext().getString(R.string.portnumber_default_implicit);
        impicitPort.setOnPreferenceChangeListener((preference, newValue) -> {
            String iport = (String) newValue;
            if (iport.isEmpty()) iport = implicitPortDefault;
            preference.setSummary(iport);
            CheckBoxPreference enableImplicit = findPref("enableImplicitPort");
            if (enableImplicit.isChecked()) enableImplicit.setSummary((String) newValue);
            FsService.restart();
            return true;
        });
        impicitPort.setSummary(FsSettings.getImplicitPortString());

        // Enables use of the TLS implicit port so that users can keep this port disabled if not using it.
        CheckBoxPreference enableImplicit = findPref("enableImplicitPort");
        enableImplicit.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                String pni = FsSettings.getImplicitPortString();
                preference.setSummary(pni);
            } else {
                preference.setSummary("Use explicit");
            }
            FsService.restart();
            return true;
        });
        String enableImplicitSum = FsSettings.getImplicitPortString();
        if (enableImplicitSum.isEmpty()) enableImplicitSum = "Use explicit";
        else enableImplicit.setSummary(enableImplicitSum);
        enableImplicit.setSummary(enableImplicitSum);

        // Allows the user to import the keystore certificate
        CheckBoxPreference certKeystore = findPref("certKeyStore");
        certKeystore.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                startIntentForCertificateFile(PICK_CERT_FILE_JKS);
            } else {
                FTPSSockets.deleteKeyStore();
                certKeystore.setSummary(getString(R.string.found_x));
                EditTextPreference certPass = findPref("certPassword");
                String passSum = certPass.getSummary().toString();
                if (passSum.contains(getString(R.string.found_check_green))) {
                    passSum = passSum.replace(getString(R.string.found_check_green),
                            getString(R.string.found_check));
                }
                certPass.setSummary(passSum);
                FsService.restart();
            }
            return true;
        });
        if (isCertFileFound("storej.jks")) {
            certKeystore.setSummary(getString(R.string.found_check));
        } else {
            certKeystore.setChecked(false);
            certKeystore.setSummary(getString(R.string.found_x));
        }

        // Allows the user to import the trust store certificate
        CheckBoxPreference certTrustStore = findPref("certTrustStore");
        certTrustStore.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                startIntentForCertificateFile(PICK_CERT_FILE_BKS);
            } else {
                FTPSSockets.deleteTrustStore();
                certTrustStore.setSummary(getString(R.string.found_x));
                FsService.restart();
            }
            return true;
        });
        if (isCertFileFound("storeb.bks")) {
            certTrustStore.setSummary(getString(R.string.found_check));
        } else {
            certTrustStore.setChecked(false);
            certTrustStore.setSummary(getString(R.string.found_x));
        }

        // Allows user to input the certificate password
        boolean keyCertGood = FTPSSockets.checkKeyStore();
        boolean trustCertGood = FTPSSockets.checkTrustStore();
        String certCheckS = getString(R.string.found_check_green);
        EditTextPreference certPass = findPref("certPassword");
        certPass.setSummary(sp.getString("certPassStar", ""));
        certPass.setOnPreferenceChangeListener((preference, newValue) -> {
            String pass = (String) newValue;
            FTPSSockets.putCertPass(pass);

            boolean keyCertGood2 = FTPSSockets.checkKeyStore();
            boolean trustCertGood2 = FTPSSockets.checkTrustStore();
            if (keyCertGood2) certKeystore.setSummary(certCheckS);
            if (trustCertGood2) certTrustStore.setSummary(certCheckS);
            else certTrustStore.setSummary(getString(R.string.found_check));

            StringBuilder sHidden = new StringBuilder();
            for (int i = 0; i < pass.length(); i++) {
                sHidden.append("*");
            }
            sp.edit().putString("certPassStar", sHidden.toString()).apply();
            certPass.setText(""); // Don't keep here. It goes to encrypted pref. Show can add back.
            if (keyCertGood2) {
                certPass.setSummary(certCheckS + " " + sHidden);
            } else {
                certPass.setSummary(getString(R.string.found_check) + " " + sHidden);
                certKeystore.setSummary(getString(R.string.found_check));
            }
            FsService.restart();
            return true;
        });
        if (keyCertGood) {
            certKeystore.setSummary(certCheckS);
            String currentCertPassS = "";
            if (certPass.getSummary() != null) currentCertPassS = certPass.getSummary().toString();
            certPass.setSummary(certCheckS + " " + currentCertPassS);
        } else {
            certPass.setSummary(getString(R.string.found_check) + " " + sp.getString("certPassStar", ""));
        }
        if (trustCertGood) {
            certTrustStore.setSummary(certCheckS);
        }

        // Shows and hides the certificate password in the UI as well as scrubbing from plain text.
        CheckBoxPreference certShowPass = findPref("certShowPassword");
        certShowPass.setOnPreferenceChangeListener((preference, newValue) -> {
            String checkMark;
            if (FTPSSockets.checkKeyStore()) checkMark = certCheckS;
            else if (FTPSSockets.getCertPass().length > 0)
                checkMark = getString(R.string.found_check);
            else checkMark = getString(R.string.found_x);
            if ((boolean) newValue) {
                certPass.setSummary(checkMark + " " + new String(FTPSSockets.getCertPass()));
                certPass.setText(new String(FTPSSockets.getCertPass()));
                certPass.getEditText().setInputType(EditorInfo.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD);
            } else {
                certPass.setSummary(checkMark + " " + sp.getString("certPassStar", ""));
                certPass.setText("");
                certPass.getEditText().setInputType(InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD);
            }
            return true;
        });
        certShowPass.setChecked(false); // Default is hidden so keep it correctly checked.

        // Enables need of the client certificate.
        CheckBoxPreference useClientCert = findPref("useClientCert");
        if (Build.VERSION.SDK_INT <= 23) {
            // Don't enable on Android 6 to avoid confusion there.
            useClientCert.setChecked(false);
            useClientCert.setEnabled(false);
        }
        useClientCert.setOnPreferenceChangeListener((preference, newValue) -> {
            FsService.restart();
            return true;
        });

        // The allow/deny list where users can select to allow or deny IPs.
        MultiSelectListPreference ipList = findPref("ip_list");
        Set<String> list = FsSettings.getIPList();
        Set<String> allowList = FsSettings.getAllowList();
        CharSequence[] cs = new CharSequence[0];
        cs = list.toArray(cs);
        Arrays.sort(cs);
        ipList.setEntries(cs);
        ipList.setEntryValues(cs);
        ipList.setDefaultValue(allowList);
        ipList.setOnPreferenceChangeListener((preference, newValue) -> {
            HashSet<CharSequence> csNew = (HashSet<CharSequence>) newValue;
            Set<String> failList = FsSettings.getFailList();
            Set<String> newList = new ArraySet<>();
            newList.addAll(failList);
            boolean failListSave = false;
            ArraySet<String> allows = new ArraySet<>();
            for (CharSequence newValueCS : csNew) {
                String newValueS = newValueCS.toString();
                allows.add(newValueS);
                if (!failList.isEmpty() && failList.contains(newValueS)) {
                    // Its being allowed so remove from fail list
                    newList.remove(newValueS);
                    failListSave = true;
                }
            }
            if (failListSave) FsSettings.putFailList(newList);
            putAllowList(allows);
            return true;
        });

        // Helps the list to get updated without recreating the UI.
        PreferenceScreen prefScreenAdvanced = findPref("preference_screen_advanced");
        prefScreenAdvanced.setOnPreferenceClickListener(preference -> {
            updateIPListWithChangesFromOtherSettings();
            return true;
        });

        // Allows user to manually add an IP or a small to large group of IPs using *.
        EditTextPreference manualAddIP = findPref("manuallyAddIP");
        manualAddIP.setOnPreferenceChangeListener((preference, newValue) -> {
            Set<String> list2 = FsSettings.getIPList();
            Set<String> newList = new ArraySet<>();
            String address = (String) newValue;
            if (address.isEmpty()) return true; // don't allow empty
            if (!list2.contains(address)) {
                char c = address.charAt(0);
                if (Character.isLetterOrDigit(c)) address = File.separator + address;
                newList.add(address);
                newList.addAll(list2);
                FsSettings.putIPList(newList);
                updateIPListWithChangesFromOtherSettings();
            }
            return true;
        });

        // Clears all IPs from the allow/deny list that are not selected as allowed.
        CheckBoxPreference clearUnusedIPs = findPref("clearUnusedIPs");
        clearUnusedIPs.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                // Can simply replace with the allow list as that's all that will be left
                Set<String> allowList1 = FsSettings.getAllowList();
                FsSettings.putIPList(allowList1);
                updateIPListWithChangesFromOtherSettings();
            }
            new Handler().postDelayed(() -> clearUnusedIPs.setChecked(false), 1000);
            return true;
        });

        // Deny all IPs except for ones that are user selected as allowed.
        CheckBoxPreference denyUntil = findPref("denyUntilAllowed");
        denyUntil.setOnPreferenceChangeListener((preference, newValue) -> {
            CheckBoxPreference denyOnFailed = findPref("denyOnFailedLogins");
            if ((boolean) newValue) {
                denyOnFailed.setChecked(false);
                denyOnFailed.setEnabled(false);
            } else {
                denyOnFailed.setChecked(false);
                denyOnFailed.setEnabled(true);
            }
            return true;
        });
        if (denyUntil.isChecked()) {
            CheckBoxPreference denyOnFailed = findPref("denyOnFailedLogins");
            denyOnFailed.setChecked(false);
            denyOnFailed.setEnabled(false);
        }

        // Deny IPs when they fail on username or password too many times.
        CheckBoxPreference denyOnFailed = findPref("denyOnFailedLogins");
        denyOnFailed.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                denyUntil.setChecked(false);
                denyUntil.setEnabled(false);
            } else {
                denyUntil.setChecked(false);
                denyUntil.setEnabled(true);
            }
            return true;
        });
        if (denyOnFailed.isChecked()) {
            denyUntil.setChecked(false);
            denyUntil.setEnabled(false);
        }

        // Used to require TLS implicit only
        CheckBoxPreference disablePlainPort = findPref("disablePlainPort");
        disablePlainPort.setOnPreferenceChangeListener((preference, newValue) -> {
            CheckBoxPreference disablePlainNotExplicit = findPref("disablePlainNotExplicit");
            if ((boolean) newValue) {
                disablePlainNotExplicit.setChecked(false);
                disablePlainNotExplicit.setEnabled(false);
            } else {
                disablePlainNotExplicit.setChecked(false);
                disablePlainNotExplicit.setEnabled(true);
            }
            FsService.restart();
            return true;
        });
        if (disablePlainPort.isChecked()) {
            CheckBoxPreference disablePlainNotExplicit = findPref("disablePlainNotExplicit");
            disablePlainNotExplicit.setChecked(false);
            disablePlainNotExplicit.setEnabled(false);
        }

        // Used to disable plain connections but allow TLS explicit connections. Explicit starts off
        // as a plain connection.
        CheckBoxPreference disablePlainNotExplicit = findPref("disablePlainNotExplicit");
        disablePlainNotExplicit.setOnPreferenceChangeListener((preference, newValue) -> {
            if ((boolean) newValue) {
                disablePlainPort.setChecked(false);
                disablePlainPort.setEnabled(false);
            } else {
                disablePlainPort.setChecked(false);
                disablePlainPort.setEnabled(true);
            }
            FsService.restart();
            return true;
        });
        if (disablePlainNotExplicit.isChecked()) {
            disablePlainPort.setChecked(false);
            disablePlainPort.setEnabled(false);
        }

        MultiSelectListPreference limitTLSProtocols = findPref("limitTLSProtocols");
        final CharSequence[] tlsProtocols = FTPSSockets.getSupportedProtocols();
        limitTLSProtocols.setEntries(tlsProtocols);
        limitTLSProtocols.setEntryValues(tlsProtocols);
        limitTLSProtocols.setOnPreferenceChangeListener((preference, newValue) -> {
            HashSet<CharSequence> csNew = (HashSet<CharSequence>) newValue;
            Set<String> newList = new ArraySet<>();
            for (CharSequence newValueCS : csNew) {
                String newValueS = newValueCS.toString();
                newList.add(newValueS);
            }
            putProtocolList(newList);
            FsService.restart();
            return true;
        });
    }

    /*
     * Checks if the internal copy of the TLS certificate is found.
     * */
    private boolean isCertFileFound(String filename) {
        try {
            File cacheFile = new File(App.getAppContext().getCacheDir(), filename);
            long fileSize = cacheFile.length();
            return fileSize != 0;
        } catch (Exception e) {
            return false;
        }
    }

    /*
     * Basically a validation of the port.
     * 0 value can be used to return back to random.
     * */
    private String checkNewPortValue(String newValue, String defaultPortValue) {
        String port = newValue;
        if (port.isEmpty()) port = defaultPortValue;
        int high = Integer.parseInt(port);
        if (high < 0) port = "0"; // Negatives are not ever valid.
        return port;
    }

    /*
    * Keeping private.
    * Puts reworked set to FTPS allow deny list.
    * */
    private void putAllowList(Set<String> newList) {
        sp.edit().putStringSet("AllowIPs", newList).apply();
    }

    /*
     * Keeping private.
     * Puts reworked set to FTPS protocol list.
     * */
    private void putProtocolList(Set<String> newList) {
        sp.edit().putStringSet("FTPSProtocolList", newList).apply();
    }

    /*
     * Updates the allow/deny list pref. For use after other settings have made changes.
     * Eg Manually add an IP to the list from another setting. Have to then upload the list pref here.
     * */
    private void updateIPListWithChangesFromOtherSettings() {
        MultiSelectListPreference ipList1 = findPref("ip_list");
        Set<String> list1 = FsSettings.getIPList();
        Set<String> allowList1 = FsSettings.getAllowList();
        CharSequence[] cs1 = new CharSequence[0];
        cs1 = list1.toArray(cs1);
        Arrays.sort(cs1);
        ipList1.setEntries(cs1);
        ipList1.setEntryValues(cs1);
        ipList1.setDefaultValue(allowList1);
    }

    /*
     * Opens the Android file picker for importing the user provided TLS certificate.
     * */
    private void startIntentForCertificateFile(int requestCode) {
        Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        intent.setType("*/*");
        startActivityForResult(intent, requestCode);
    }

    /*
    * Multi user:
    * Upgrade path for previous "write external storage" use so that it doesn't break on update.
    * Populates the new uriString use of each user.
    * */
    private void writeExtMultiUserUpgradePath() {
        List<UriPermission> oldList = App.getAppContext().getContentResolver().getPersistedUriPermissions();
        if (oldList.size() == 0) return;
        Uri uri = oldList.get(0).getUri();
        if (uri == null) return;
        List<FtpUser> userList = FsSettings.getUsers();
        for (int i = 0; i < userList.size(); i++) {
            if (userList.get(i) == null) continue;
            FtpUser entry = new FtpUser(userList.get(i).getUsername(),
                    userList.get(i).getPassword(),
                    userList.get(i).getChroot(),
                    uri.getPath());
            FsSettings.modifyUser(userList.get(i).getUsername(), entry);
        }
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
            Uri treeUri = resultData.getData();
            String path = treeUri.getPath();
            Cat.d("Action Open Document Tree on path " + path);

            final CheckBoxPreference writeExternalStoragePref = findPref("writeExternalStorage");
            if (path.contains("primary:")) {
                writeExternalStoragePref.setChecked(false);
            } else {
                FsSettings.setExternalStorageUri(treeUri.toString());
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                    getActivity().getContentResolver()
                            .takePersistableUriPermission(treeUri,
                                    Intent.FLAG_GRANT_READ_URI_PERMISSION
                                            | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
                }
                writeExternalStoragePref.setChecked(true);
            }
        } else if (requestCode == PICK_CERT_FILE_JKS && resultCode == Activity.RESULT_OK) {
            Uri uri = resultData.getData();
            if (uri == null) return;
            importCertFromIntentResult(uri, "storej.jks", "certKeyStore");
        } else if (requestCode == PICK_CERT_FILE_BKS && resultCode == Activity.RESULT_OK) {
            Uri uri = resultData.getData();
            if (uri == null) return;
            importCertFromIntentResult(uri, "storeb.bks", "certTrustStore");
        }
    }

    /*
     * Imports the user provided certificate file, tests it, and provides visual working state.
     * */
    private void importCertFromIntentResult(Uri uri, String certFilename, String certPrefKey) {
        importCertFile(uri, certFilename);
        CheckBoxPreference certPref = findPref(certPrefKey);
        if (isCertFileFound(certFilename)) {
            certPref.setSummary(getString(R.string.found_check));
            if (FTPSSockets.getCertPass().length > 0) {
                if (FTPSSockets.checkTrustStore()) {
                    certPref.setSummary(getString(R.string.found_check_green));
                    if (FTPSSockets.checkKeyStore()) {
                        EditTextPreference certPass = findPref("certPassword");
                        String s = certPass.getSummary().toString();
                        s = getString(R.string.found_check_green) + s.substring(1);
                        certPass.setSummary(s);
                    }
                }
            }
            FsService.restart();
        } else {
            certPref.setChecked(false);
            certPref.setSummary(getString(R.string.found_x));
        }
    }

    /*
     * Copies the user provided certificate file to app cache
     * Just going to do on UI thread as responsiveness is great. Only a small amount of kb.
     * */
    private void importCertFile(Uri uri, String filename) {
        FileOutputStream fos = null;
        BufferedInputStream br = null;
        try (InputStream is = App.getAppContext().getContentResolver().openInputStream(uri)) {
            br = new BufferedInputStream(is);
            File cacheFile = new File(App.getAppContext().getCacheDir(), filename);
            fos = new FileOutputStream(cacheFile);
            byte[] buffer = new byte[1024];
            int count;
            while ((count = br.read(buffer)) != -1) {
                fos.write(buffer, 0, count);
            }
            br.close();
        } catch (Exception e) {
            //
        } finally {
            if (fos != null) {
                try {
                    fos.close();
                } catch (IOException e) {
                    //
                }
            }
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    //
                }
            }
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
            String ipText = "ftp://" + address.getHostAddress() + ":" + FsSettings.getPortNumber();
            if (FsSettings.isImplicitUsed()) ipText += ", " + FsSettings.getPortNumberImplicit();
            ipText += "/";
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
