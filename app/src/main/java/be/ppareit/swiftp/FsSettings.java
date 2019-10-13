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

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.io.File;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import be.ppareit.swiftp.server.FtpUser;
import lombok.val;

public class FsSettings {

    private final static String TAG = FsSettings.class.getSimpleName();

    public static List<FtpUser> getUsers() {
        final Context context = App.getAppContext();
        final SharedPreferences sp = getSharedPreferences();
        if (sp.contains("users")) {
            Gson gson = new Gson();
            Type listType = new TypeToken<List<FtpUser>>() {
            }.getType();
            return gson.fromJson(sp.getString("users", null), listType);
        } else if (sp.contains("username")) {
            // on ftp server version < 2.19 we had username/password preference
            String username = sp.getString("username", context.getString(R.string.username_default));
            String password = sp.getString("password", context.getString(R.string.password_default));
            String chroot = sp.getString("chrootDir", "");
            if (username == null || password == null || chroot == null) {
                username = context.getString(R.string.username_default);
                password = context.getString(R.string.password_default);
                chroot = "";
            }
            return new ArrayList<>(Collections.singletonList(new FtpUser(username, password, chroot)));
        } else {
            val defaultUser = new FtpUser(context.getString(R.string.username_default), context.getString(R.string.password_default), "\\");
            return new ArrayList<>(Collections.singletonList(defaultUser));
        }
    }

    public static FtpUser getUser(String username) {
        for (val user : getUsers()) {
            if (user.getUsername().equals(username))
                return user;
        }
        return null;
    }

    public static void addUser(FtpUser user) {
        if (getUser(user.getUsername()) != null) {
            throw new IllegalArgumentException("User already exists");
        }
        val sp = getSharedPreferences();
        Gson gson = new Gson();
        val userList = getUsers();
        userList.add(user);
        sp.edit().putString("users", gson.toJson(userList)).apply();
    }

    public static void removeUser(String username) {
        val sp = getSharedPreferences();
        Gson gson = new Gson();
        val users = getUsers();
        val found = new ArrayList<FtpUser>();
        for (val user : users) {
            if (user.getUsername().equals(username)) {
                found.add(user);
            }
        }
        users.removeAll(found);
        sp.edit().putString("users", gson.toJson(users)).apply();
    }

    public static void modifyUser(String username, FtpUser newUser) {
        removeUser(username);
        addUser(newUser);
    }

    public static boolean allowAnonymous() {
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
        // TODO: port is always an number, so store this accordingly
        String portString = sp.getString("portNum", "2121");
        if (portString == null) {
            portString = "2121";
        }
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

    public static void removeFromAutoConnectList(final String ssid) {
        Set<String> autoConnectList = getAutoConnectList();
        autoConnectList.remove(ssid);
        val editor = getSharedPreferences().edit();
        editor.remove("autoconnect_preference").apply(); // work around bug in android
        editor.putStringSet("autoconnect_preference", autoConnectList).apply();
    }

    public static int getTheme() {
        SharedPreferences sp = getSharedPreferences();
        String theme = sp.getString("theme", "0");
        if (theme == null) {
            return R.style.AppThemeDark;
        }

        switch (theme) {
            case "0":
                return R.style.AppThemeDark;
            case "1":
                return R.style.AppThemeLight;
            case "2":
                return R.style.AppThemeLight_DarkActionBar;
            default:
                return R.style.AppThemeDark;
        }

    }

    public static boolean showNotificationIcon() {
        val sp = getSharedPreferences();
        return sp.getBoolean("show_notification_icon_preference", true);
    }

    /**
     * @return the SharedPreferences for this application
     */
    private static SharedPreferences getSharedPreferences() {
        final Context context = App.getAppContext();
        return PreferenceManager.getDefaultSharedPreferences(context);
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
