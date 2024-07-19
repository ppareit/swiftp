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

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.FileUtil;

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
            FtpUser defaultUser = new FtpUser(context.getString(R.string.username_default), context.getString(R.string.password_default), "\\");
            return new ArrayList<>(Collections.singletonList(defaultUser));
        }
    }

    public static FtpUser getUser(String username) {
        for (FtpUser user : getUsers()) {
            if (user.getUsername().equals(username))
                return user;
        }
        return null;
    }

    public static void addUser(FtpUser user) {
        if (getUser(user.getUsername()) != null) {
            throw new IllegalArgumentException("User already exists");
        }
        SharedPreferences sp = getSharedPreferences();
        Gson gson = new Gson();
        List<FtpUser> userList = getUsers();
        userList.add(user);
        sp.edit().putString("users", gson.toJson(userList)).apply();
    }

    public static void removeUser(String username) {
        SharedPreferences sp = getSharedPreferences();
        Gson gson = new Gson();
        List<FtpUser> users = getUsers();
        ArrayList<FtpUser> found = new ArrayList<>();
        for (FtpUser user : users) {
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
        // Get the path from the app's MANAGE USERS chroot folder UI text field that the user will use during setup.
        String subFix = null;
        if (Util.useScopedStorage()) {
            // The app's MANAGE USERS chroot folder UI selection cannot select the sd card at least on Android 11+.
            //  The picker does all that's needed there so that use should be switched with ADVANCED SETTINGS >
            //  WRITE EXTERNAL picker or just make it invisible when on A11+ ? Could also just pull open the same
            //  picker on both with A11+ or something else. It also presents possible conflicts with the Uri path
            //  eg "/storage/sd card/" verses "/sd card/Test/".
            String s = FileUtil.cleanupUriStoragePath(FileUtil.getTreeUri());
            if (s != null && !s.contains("primary:")) {
                final String chroot = FileUtil.getSdCardBaseFolderScopedStorage();
                // Need to return eg "/storage" for sd card and "/storage/emulated/0" for internal.
                if (chroot != null && !chroot.isEmpty()) return new File(chroot);
                // otherwise just get the other path from below.
            } else if (s != null && s.contains("primary:")) {
                // Fix for issue seen on Android 8.0:
                // Had to implement over below as the below chroot is forced to
                // getExternalStorageDirectory() when actual chroot may include further sub dirs.
                subFix = s.replace("primary:", "");
                // At the moment, have to do it below, as a StackOverflow is happening on the test device
                // here with any additional code for an unknown reason.
            }
        }

        // Original below incorrectly returns "/storage/emulated/0" for sd card with Android 11+
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
        if (!chrootDir.canRead() || !chrootDir.canWrite()) {
            Cat.e("We cannot read/write in the default directory, changing to application directory");
            return App.getAppContext().getFilesDir();
        }

        if (subFix != null) return new File(chrootDir, subFix);
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

    public static String getBatterySaverChoice(String val) {
        final SharedPreferences sp = getSharedPreferences();
        String s = sp.getString("battery_saver", "1");
        if (val != null) s = val;
        if (s.equals("0")) return  App.getAppContext().getString(R.string.bs_high);
        if (s.equals("1")) return App.getAppContext().getString(R.string.bs_low);
        return App.getAppContext().getString(R.string.bs_deep);
    }

    public static boolean shouldTakeFullWakeLock() {
        final SharedPreferences sp = getSharedPreferences();
        return sp.getBoolean("stayAwake", false);
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
        SharedPreferences sp = getSharedPreferences();
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
