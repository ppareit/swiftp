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
