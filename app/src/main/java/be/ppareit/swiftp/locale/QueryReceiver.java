package be.ppareit.swiftp.locale;

import android.content.Context;
import android.os.Bundle;
import android.support.annotation.NonNull;

import com.twofortyfouram.locale.sdk.client.receiver.AbstractPluginConditionReceiver;

/**
 * Created by ppareit on 29/04/16.
 */
public class QueryReceiver extends AbstractPluginConditionReceiver {
    @Override
    protected boolean isBundleValid(@NonNull Bundle bundle) {
        return false;
    }

    @Override
    protected boolean isAsync() {
        return false;
    }

    @Override
    protected int getPluginConditionResult(@NonNull Context context, @NonNull Bundle bundle) {
        return 0;
    }
}
