// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp;

import android.content.Context;
import android.media.MediaScannerConnection;
import android.net.Uri;
import android.util.Log;

/**
 * This media scanner runs in the background. The rescan might
 * not happen immediately.
 */
public enum MediaUpdater {
    INSTANCE;

    private final static String TAG = MediaUpdater.class.getSimpleName();

    private static class ScanCompletedListener implements
            MediaScannerConnection.OnScanCompletedListener {
        @Override
        public void onScanCompleted(String path, Uri uri) {
            Log.i(TAG, "Scan completed: " + path + " : " + uri);
        }
    }

    public static void notifyFileCreated(String path) {
        Log.d(TAG, "Notifying others about new file: " + path);
        Context context = App.getAppContext();
        MediaScannerConnection.scanFile(context, new String[] { path }, null,
                new ScanCompletedListener());
    }

    public static void notifyFileDeleted(String path) {
        Log.d(TAG, "Notifying others about deleted file: " + path);
        Context context = App.getAppContext();
        MediaScannerConnection.scanFile(context, new String[] { path }, null,
                new ScanCompletedListener());
    }
}
