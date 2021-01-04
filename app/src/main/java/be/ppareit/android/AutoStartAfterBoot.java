package be.ppareit.android;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;

public class AutoStartAfterBoot  extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if (Intent.ACTION_BOOT_COMPLETED.equals(intent.getAction()) && FsSettings.allowAfterBootStart()) {
            FsService.start();
        }
    }
}