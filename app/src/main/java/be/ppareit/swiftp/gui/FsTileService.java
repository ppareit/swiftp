// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.gui;

import android.content.BroadcastReceiver;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.os.SystemClock;
import android.service.quicksettings.Tile;
import android.service.quicksettings.TileService;
import androidx.annotation.RequiresApi;
import androidx.core.content.ContextCompat;

import net.vrallev.android.cat.Cat;

import java.net.InetAddress;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;

import static be.ppareit.android.BroadcastReceiverUtils.createBroadcastReceiver;

@RequiresApi(api = Build.VERSION_CODES.N)
public class FsTileService extends TileService {

    /** How long a failed start stays on the tile  */
    private static final long FAILURE_SHOWN_MS = 10_000;

    /** String id of the last start failure, or 0 for none. */
    private int failure = 0;
    private long failureAt = 0;

    private final Handler handler = new Handler(Looper.getMainLooper());

    /** Takes the reason back off the tile, for a panel the user leaves open. */
    private final Runnable clearFailure = () -> {
        failure = 0;
        updateTileState();
    };

    @Override
    public void onClick() {
        if (isSecure()) unlockAndRun(this::clicked);
        else clicked();
    }

    private void clicked() {
        if (getQsTile().getState() == Tile.STATE_INACTIVE)
            FsService.start();
        else if (getQsTile().getState() == Tile.STATE_ACTIVE)
            FsService.stop();
    }

    @Override
    public void onStartListening() {
        IntentFilter intentFilter = new IntentFilter();
        intentFilter.addAction(FsService.ACTION_STARTED);
        intentFilter.addAction(FsService.ACTION_STOPPED);
        intentFilter.addAction(FsService.ACTION_FAILEDTOSTART);

        // Our own actions only, so no other app can drive the tile state.
        ContextCompat.registerReceiver(this, mFsActionsReceiver, intentFilter,
                ContextCompat.RECEIVER_NOT_EXPORTED);

        updateTileState();
    }

    @Override
    public void onStopListening() {
        // getQsTile() is null once we stop listening, so nothing may repaint after this.
        handler.removeCallbacks(clearFailure);
        unregisterReceiver(mFsActionsReceiver);
    }

    private void updateTileState() {
        Tile tile = getQsTile();
        if (tile == null) return;  // not listening, the panel is closed
        if (!FsService.isRunning() && showingFailure()) {
            tile.setState(Tile.STATE_INACTIVE);
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
                // The subtitle takes the place of On/Off, so the label stays the app name.
                tile.setLabel(getString(R.string.swiftp_name));
                tile.setSubtitle(getString(failure));
            } else {
                tile.setLabel(getString(failure));
            }
            tile.updateTile();
            return;
        }
        failure = 0;
        if (FsService.isRunning()) {
            tile.setState(Tile.STATE_ACTIVE);
            setSubtitle(tile, getString(R.string.tile_on));
            // Fill in the FTP server address
            InetAddress address = FsService.getLocalInetAddress();
            if (address == null) {
                Cat.v("Unable to retrieve wifi ip address");
                tile.setLabel(getString(R.string.swiftp_name));
                tile.updateTile();
                return;
            }
            tile.setLabel(address.getHostAddress() + ":" + FsSettings.getPortNumber());
        } else {
            tile.setState(Tile.STATE_INACTIVE);
            setSubtitle(tile, getString(R.string.tile_off));
            tile.setLabel(getString(R.string.swiftp_name));
        }
        tile.updateTile();
    }

    /**
     * A failed start used to leave the tile silently back on its app name, so tapping it looked
     * like nothing happened at all. Say why on the tile itself, and take it back off again: the
     * panel the user tapped in usually stays open, and nothing else would repaint the tile.
     */
    private void showFailure(Intent intent) {
        switch (intent.getIntExtra(FsService.EXTRA_FAILURE, FsService.FAILURE_NO_NETWORK)) {
            case FsService.FAILURE_MOBILE_ONLY:
                failure = R.string.tile_failed_mobile_only;
                break;
            case FsService.FAILURE_PORT:
                failure = R.string.tile_failed_port;
                break;
            default:
                failure = R.string.tile_failed_no_network;
                break;
        }
        failureAt = SystemClock.elapsedRealtime();
        updateTileState();
        handler.removeCallbacks(clearFailure);
        handler.postDelayed(clearFailure, FAILURE_SHOWN_MS);
    }

    /** The subtitle is where a tile says On or Off, and it only exists since Q. */
    private static void setSubtitle(Tile tile, String subtitle) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) tile.setSubtitle(subtitle);
    }

    private boolean showingFailure() {
        return failure != 0 && SystemClock.elapsedRealtime() - failureAt < FAILURE_SHOWN_MS;
    }

    BroadcastReceiver mFsActionsReceiver = createBroadcastReceiver(
            (context, intent) -> {
                if (FsService.ACTION_FAILEDTOSTART.equals(intent.getAction())) showFailure(intent);
                else updateTileState();
            }
    );
}
