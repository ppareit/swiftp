package be.ppareit.swiftp.locale;

import android.content.Context;
import android.os.Bundle;
import android.support.annotation.NonNull;

import com.twofortyfouram.assertion.BundleAssertions;
import com.twofortyfouram.spackle.AppBuildInfo;

import net.vrallev.android.cat.Cat;

public final class SettingsBundleHelper {

    public static final String BUNDLE_BOOLEAN_RUNNING = "be.ppareit.swiftp.BOOLEAN_RUNNING";
    public static final String BUNDLE_VERSION_CODE = "be.ppareit.swiftp.VERSION_CODE";


    static public boolean isBundleValid(Bundle bundle) {
        if (bundle == null) {
            return false;
        }
        try {
            BundleAssertions.assertHasBoolean(bundle, BUNDLE_BOOLEAN_RUNNING);
            BundleAssertions.assertHasInt(bundle, BUNDLE_VERSION_CODE);
        } catch (AssertionError e) {
            Cat.e("Bundle failed verification");
            return false;
        }
        return true;
    }

    static public Bundle generateBundle(Context context, boolean running) {
        Bundle bundle = new Bundle();
        bundle.putBoolean(BUNDLE_BOOLEAN_RUNNING, running);
        bundle.putInt(BUNDLE_VERSION_CODE, AppBuildInfo.getVersionCode(context));
        return bundle;
    }

    static public boolean getBundleRunningState(@NonNull Bundle bundle) {
        return bundle.getBoolean(BUNDLE_BOOLEAN_RUNNING, false);
    }

    private SettingsBundleHelper() {
    }

}
