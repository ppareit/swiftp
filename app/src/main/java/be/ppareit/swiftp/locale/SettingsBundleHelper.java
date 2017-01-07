/*
Copyright 2016-2017 Pieter Pareit

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
