package be.ppareit.swiftp;

import android.app.Application;
import android.content.Context;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.util.Log;

public class FTPServerApplication extends Application {

    private static final String TAG = FTPServerApplication.class.getSimpleName();

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
        if (sContext == null)
            Log.e(TAG, "Global context not set");
        return sContext;
    }

    /**
     * @return true if this is the free version
     */
    public static boolean isFreeVersion() {
        try {
            return Globals.getContext().getPackageName().contains("free");
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
