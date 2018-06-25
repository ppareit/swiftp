/*
Copyright 2011-2013 Pieter Pareit
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
*/

package be.ppareit.swiftp;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Environment;
import android.preference.PreferenceManager;
import android.util.Log;

import net.vrallev.android.cat.Cat;

import org.json.JSONArray;
import org.json.JSONException;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import be.ppareit.swiftp.server.FtpUser;

public class FsSettings {

    private final static String TAG = FsSettings.class.getSimpleName();

    private static JSONArray getUserArray() throws JSONException {
        final SharedPreferences sp = getSharedPreferences();
        return new JSONArray(sp.getString("users", "[ [\"ftp\", \"ftp\", null] ]"));
    }

    public static List<FtpUser> listAllUsers() {
        try {
            JSONArray _users = getUserArray();
            List<FtpUser> users = new ArrayList<>(_users.length());
            JSONArray details;
            for (int i = _users.length() - 1; i >= 0; i--) {
                details = _users.getJSONArray(i);
                users.add(new FtpUser(details.getString(0), details.getString(1), details.getString(2)));
            }
            return users;
        } catch (JSONException e) {
            return new ArrayList<>();
        }
    }

    public static FtpUser getUser(String username) {
        try {
            JSONArray _users = getUserArray();
            JSONArray details;
            for (int i = 0; i < _users.length(); i++) {
                details = _users.getJSONArray(i);
                if (details.getString(0).equals(username)) {
                    return new FtpUser(username, details.getString(1), details.getString(2));
                }
            }
        } catch (JSONException e) {
            Cat.w(e);
        }
        return null;
    }

    public static void addUser(FtpUser user) {
        if (getUser(user.getUsername()) != null) {
            throw new IllegalArgumentException("User already exists");
        }
        try {
            JSONArray _users = getUserArray();
            JSONArray details = new JSONArray();
            details.put(user.getUsername());
            details.put(user.getPassword());
            details.put(user.getChroot());
            _users.put(details);
            getSharedPreferences().edit().putString("users", _users.toString()).apply();
        } catch (JSONException e) {
            Cat.w(e);
        }
    }

    public static void removeUser(String username) {
        try {
            JSONArray _users = getUserArray();
            JSONArray updatedUsers = new JSONArray();
            JSONArray details;
            for (int i = 0; i < _users.length(); i++) {
                details = _users.getJSONArray(i);
                if (!details.getString(0).equals(username)) {
                    updatedUsers.put(details);
                }
            }
            getSharedPreferences().edit().putString("users", updatedUsers.toString()).apply();
        } catch (JSONException e) {
            e.printStackTrace();
        }
    }

    public static void modifyUser(String username, FtpUser newUser) {
        try {
            JSONArray _users = getUserArray();
            JSONArray updatedUsers = new JSONArray();
            JSONArray details;
            for (int i = 0; i < _users.length(); i++) {
                details = _users.getJSONArray(i);
                if (details.getString(0).equals(username)) {
                    details = new JSONArray();
                    details.put(newUser.getUsername());
                    details.put(newUser.getPassword());
                    details.put(newUser.getChroot());
                }
                updatedUsers.put(details);
            }
            getSharedPreferences().edit().putString("users", updatedUsers.toString()).apply();
        } catch (JSONException e) {
            Cat.w(e);
        }
    }

    public static boolean allowAnoymous() {
        final SharedPreferences sp = getSharedPreferences();
        return sp.getBoolean("allow_anonymous", false);
    }

    public static File getDefaultChrootDir() {
        File chrootDir;
        if (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED)) {
            chrootDir = Environment.getExternalStorageDirectory();
        } else {
            chrootDir = new File("/");
        }
        if (!chrootDir.isDirectory()) {
            Log.e(TAG, "getChrootDir: not a directory");
            // if this happens, we are screwed
            // we give it the application directory
            // but this will probably not be what the user wants
            return App.getAppContext().getFilesDir();
        }
        return chrootDir;
    }

    public static int getPortNumber() {
        final SharedPreferences sp = getSharedPreferences();
        // TODO: port is always an number, so store this accordenly
        String portString = sp.getString("portNum", "2121");
        int port = Integer.valueOf(portString);
        Log.v(TAG, "Using port: " + port);
        return port;
    }

    public static boolean shouldTakeFullWakeLock() {
        final SharedPreferences sp = getSharedPreferences();
        return sp.getBoolean("stayAwake", false);
    }

    public static Set<String> getAutoConnectList() {
        SharedPreferences sp = getSharedPreferences();
        return sp.getStringSet("autoconnect_preference", new TreeSet<>());
    }

    public static int getTheme() {
        SharedPreferences sp = getSharedPreferences();

        switch (sp.getString("theme", "0")) {
            case "0":
                return R.style.AppThemeDark;
            case "1":
                return R.style.AppThemeLight;
            case "2":
                return R.style.AppThemeLight_DarkActionBar;
        }

        return R.style.AppThemeDark;
    }

    /**
     * @return the SharedPreferences for this application
     */
    private static SharedPreferences getSharedPreferences() {
        final Context context = App.getAppContext();
        return PreferenceManager.getDefaultSharedPreferences(context);
    }

    // cleaning up after his
    protected static int inputBufferSize = 256;
    protected static boolean allowOverwrite = false;
    protected static int dataChunkSize = 8192; // do file I/O in 8k chunks
    protected static int sessionMonitorScrollBack = 10;
    protected static int serverLogScrollBack = 10;

    public static int getInputBufferSize() {
        return inputBufferSize;
    }

    public static void setInputBufferSize(int inputBufferSize) {
        FsSettings.inputBufferSize = inputBufferSize;
    }

    public static boolean isAllowOverwrite() {
        return allowOverwrite;
    }

    public static void setAllowOverwrite(boolean allowOverwrite) {
        FsSettings.allowOverwrite = allowOverwrite;
    }

    public static int getDataChunkSize() {
        return dataChunkSize;
    }

    public static void setDataChunkSize(int dataChunkSize) {
        FsSettings.dataChunkSize = dataChunkSize;
    }

    public static int getSessionMonitorScrollBack() {
        return sessionMonitorScrollBack;
    }

    public static void setSessionMonitorScrollBack(int sessionMonitorScrollBack) {
        FsSettings.sessionMonitorScrollBack = sessionMonitorScrollBack;
    }

    public static int getServerLogScrollBack() {
        return serverLogScrollBack;
    }

    public static void setLogScrollBack(int serverLogScrollBack) {
        FsSettings.serverLogScrollBack = serverLogScrollBack;
    }

    public static String getExternalStorageUri() {
        final SharedPreferences sp = getSharedPreferences();
        return sp.getString("externalStorageUri", null);
    }

    public static void setExternalStorageUri(String externalStorageUri) {
        final SharedPreferences sp = getSharedPreferences();
        sp.edit().putString("externalStorageUri", externalStorageUri).apply();
    }

}
