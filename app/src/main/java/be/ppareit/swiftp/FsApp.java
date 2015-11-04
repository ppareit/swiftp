/*
Copyright 2011-2013 Pieter Pareit

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

import android.app.Application;
import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;

public class FsApp extends Application {

    private static final String TAG = FsApp.class.getSimpleName();

    private static Context sContext;

    @Override
    public void onCreate() {
        super.onCreate();
        sContext = getApplicationContext();
    }

    /**
     * @return the Context of this application
     */
    public static Context getAppContext() {
        if (sContext == null) {
            Log.e(TAG, "Global context not set");
        }
        return sContext;
    }

    /**
     * @return true if this is the free version
     */
    public static boolean isFreeVersion() {
        try {
            Context context = getAppContext();
            return context.getPackageName().contains("free");
        } catch (Exception swallow) {
        }
        return false;
    }

    /**
     * Get the version from the manifest.
     * 
     * @return The version as a String.
     */
    public static String getVersion() {
        Context context = getAppContext();
        String packageName = context.getPackageName();
        try {
            PackageManager pm = context.getPackageManager();
            return pm.getPackageInfo(packageName, 0).versionName;
        } catch (NameNotFoundException e) {
            Log.e(TAG, "Unable to find the name " + packageName + " in the package");
            return null;
        }
    }

}
