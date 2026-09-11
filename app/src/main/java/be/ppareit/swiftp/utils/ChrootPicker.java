// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.os.Environment;
import android.widget.Toast;

import androidx.annotation.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import be.ppareit.swiftp.R;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.gui.FolderPickerDialogBuilder;

public class ChrootPicker {

    public ChrootPicker() {
    }

    public interface OnTextEventListener {
        void OnEvent(String s);
    }

    public OnTextEventListener onTextEventListener;

    public void setOnTextEventListener(OnTextEventListener onTextEventListener) {
        this.onTextEventListener = onTextEventListener;
    }

    private boolean isShowingFolderPicker = false;

    public void showFolderPicker(String s, @Nullable Activity a, Context fragment /*Fragment use*/) {
        if (Util.useScopedStorage()) {
            // Under SAF listFiles() returns null, so browsing shows nothing. Offer the granted
            // folders and their root instead, exactly the set of chroots that can work.
            showAllowedFolderChoice(a != null ? a : fragment);
            return;
        }
        if (isShowingFolderPicker)
            return;
        isShowingFolderPicker = true;
        final File startDir;
        if (s.isEmpty()) {
            startDir = Environment.getExternalStorageDirectory();
        } else {
            startDir = new File(s);
        }
        AlertDialog folderPicker = new FolderPickerDialogBuilder(a != null ? a : fragment, startDir)
                .setSelectedButton(R.string.select, path -> {
                    final File root = new File(path);
                    if (!root.canRead()) {
                        showToast(R.string.notice_cant_read_write,
                                a != null ? a : fragment);
                    } else if (!root.canWrite()) {
                        showToast(R.string.notice_cant_write,
                                a != null ? a : fragment);
                    }
                    if (onTextEventListener != null) onTextEventListener.OnEvent(path);
                })
                .setNegativeButton(R.string.cancel, null)
                .create();
        folderPicker.setOnDismissListener(dialog -> isShowingFolderPicker = false);
        folderPicker.show();
    }

    /**
     * The chroots that can work under SAF: one of the allowed folders. Anything else would look
     * fine in the UI and then serve nothing.
     *
     * The directory they are all listed under is not offered any more: that is the whole set,
     * which is what "All allowed folders" says one step earlier, and saying it as a path is
     * what stopped following the folders in the first place.
     */
    private void showAllowedFolderChoice(Context context) {
        if (isShowingFolderPicker) return;

        final List<String> choices = new ArrayList<>(AllowedFolders.paths());

        if (choices.isEmpty()) {
            showToast(R.string.allowed_folders_none_chosen, context);
            return;
        }

        isShowingFolderPicker = true;
        final String[] items = choices.toArray(new String[0]);
        AlertDialog dialog = new AlertDialog.Builder(context)
                .setTitle(R.string.allowed_folders_title)
                .setItems(items, (d, which) -> {
                    if (onTextEventListener != null) onTextEventListener.OnEvent(items[which]);
                })
                .setNegativeButton(R.string.cancel, null)
                .create();
        dialog.setOnDismissListener(d -> isShowingFolderPicker = false);
        dialog.show();
    }

    private void showToast(int errorResId, Context context) {
        Toast.makeText(context, errorResId, Toast.LENGTH_LONG).show();
    }
}
