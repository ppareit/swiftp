package be.ppareit.swiftp.locale;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;

import com.twofortyfouram.locale.sdk.client.receiver.AbstractPluginSettingReceiver;

import be.ppareit.swiftp.FsService;

import static be.ppareit.swiftp.locale.SettingsBundleHelper.getBundleRunningState;

/**
 * Created by ppareit on 29/04/16.
 */
public class FireReceiver extends AbstractPluginSettingReceiver {
    @Override
    protected boolean isBundleValid(@NonNull Bundle bundle) {
        return SettingsBundleHelper.isBundleValid(bundle);
    }

    @Override
    protected boolean isAsync() {
        return false;
    }

    @Override
    protected void firePluginSetting(@NonNull Context context, @NonNull Bundle bundle) {
        boolean running = getBundleRunningState(bundle);
        if (running && !FsService.isRunning()) {
            context.sendBroadcast(new Intent(FsService.ACTION_START_FTPSERVER));
        } else if (!running && FsService.isRunning()) {
            context.sendBroadcast(new Intent(FsService.ACTION_STOP_FTPSERVER));
        }
    }
}
