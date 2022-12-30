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

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Build;
import android.preference.PreferenceManager;
import android.util.Log;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

abstract public class Util {
    final static String TAG = Util.class.getSimpleName();
    private static SharedPreferences sp = null;
    private static boolean overrideSDKVer = false;

    public static byte byteOfInt(int value, int which) {
        int shift = which * 8;
        return (byte) (value >> shift);
    }

    public static String ipToString(int address, String sep) {
        if (address > 0) {
            StringBuffer buf = new StringBuffer();
            buf.append(byteOfInt(address, 0)).append(sep).append(byteOfInt(address, 1))
                    .append(sep).append(byteOfInt(address, 2)).append(sep)
                    .append(byteOfInt(address, 3));
            Log.d(TAG, "ipToString returning: " + buf.toString());
            return buf.toString();
        } else {
            return null;
        }
    }

    public static InetAddress intToInet(int value) {
        byte[] bytes = new byte[4];
        for (int i = 0; i < 4; i++) {
            bytes[i] = byteOfInt(value, i);
        }
        try {
            return InetAddress.getByAddress(bytes);
        } catch (UnknownHostException e) {
            // This only happens if the byte array has a bad length
            return null;
        }
    }

    public static String ipToString(int address) {
        if (address == 0) {
            // This can only occur due to an error, we shouldn't blindly
            // convert 0 to string.
            Log.e(TAG, "ipToString won't convert value 0");
            return null;
        }
        return ipToString(address, ".");
    }

    public static String[] concatStrArrays(String[] a1, String[] a2) {
        String[] retArr = new String[a1.length + a2.length];
        System.arraycopy(a1, 0, retArr, 0, a1.length);
        System.arraycopy(a2, 0, retArr, a1.length, a2.length);
        return retArr;
    }

    public static void sleepIgnoreInterrupt(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException ignored) {
        }
    }

    /**
     * Creates a SimpleDateFormat in the formatting used by ftp sever/client.
     */
    private static SimpleDateFormat createSimpleDateFormat() {
        SimpleDateFormat df = new SimpleDateFormat("yyyyMMddHHmmss", Locale.US);
        df.setTimeZone(TimeZone.getTimeZone("UTC"));
        return df;
    }

    public static String getFtpDate(long time) {
        SimpleDateFormat df = createSimpleDateFormat();
        return df.format(new Date(time));
    }

    public static Date parseDate(String time) throws ParseException {
        SimpleDateFormat df = createSimpleDateFormat();
        return df.parse(time);
    }

    /*
     * Implemented mainly for Android 11+ where its forced but can work on earlier Android versions.
     * Uses an override for when File fails to work during a test as the user sets up the app.
     * */
    public static boolean useScopedStorage() {
        if (sp == null) {
            sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
            overrideSDKVer = sp.getBoolean("OverrideScopedStorageMinimum", false);
        }
        return Build.VERSION.SDK_INT >= 30 || overrideSDKVer;
    }

    public static void reGetStorageOverride() {
        sp = null;
    }
}
