package be.ppareit.swiftp.utils;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.widget.Toast;

import androidx.annotation.Nullable;
import androidx.documentfile.provider.DocumentFile;

import java.io.File;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.gui.FolderPickerDialogBuilder;

public class ChrootPicker {

    public ChrootPicker() {
    }

    public interface OnTextEventListener {
        void OnEvent(String s);
    }

    public interface OnActionEventListener {
        void OnEvent();
    }

    public OnTextEventListener onTextEventListener;
    public OnActionEventListener onActionEventListener;

    public void setOnTextEventListener(OnTextEventListener onTextEventListener) {
        this.onTextEventListener = onTextEventListener;
    }

    public void setOnActionTreeEventListener(OnActionEventListener onActionEventListener) {
        this.onActionEventListener = onActionEventListener;
    }

    private boolean isShowingFolderPicker = false;

    public void save(Context context, Uri treeUri) {
        // *************************************
        // The order following here is critical. They must stay ordered as they are.
        setPermissionToUseExternalStorage(treeUri, context);
        scopedStorageChrootOverride(treeUri);
    }

    private void setPermissionToUseExternalStorage(Uri treeUri, Context context) {
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                FsSettings.setExternalStorageUri(treeUri.toString());
                context.getContentResolver()
                        .takePersistableUriPermission(treeUri,
                                Intent.FLAG_GRANT_READ_URI_PERMISSION
                                        | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
            }
        } catch (SecurityException e) {
            // Harden code against crash: May reach here by adding exact same picker location but
            // being removed at same time.
        }
    }

    private void scopedStorageChrootOverride(Uri treeUri) {
        if (Util.useScopedStorage()) {
            DocumentFile df = FileUtil.getDocumentFileFromUri(treeUri);
            if (df == null) return;
            String newPath = "/storage/emulated/0/";
            String treePath = treeUri.getPath();
            if (treePath == null) return;
            if (treePath.contains("primary:"))
                treePath = treePath.substring(treePath.indexOf(":") + 1);
            else if (treePath.contains(":")) {
                newPath = "/storage/";
                treePath = treePath.replace("/tree/", "");
                treePath = treePath.replace(":", "/");
            }
            newPath += treePath;
            if (onTextEventListener != null) onTextEventListener.OnEvent(newPath);
        }
    }

    public void showFolderPicker(String s, @Nullable Activity a, Context fragment /*Fragment use*/) {
        if (Util.useScopedStorage()) {
            if (onActionEventListener != null) onActionEventListener.OnEvent();
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

    private void showToast(int errorResId, Context context) {
        Toast.makeText(context, errorResId, Toast.LENGTH_LONG).show();
    }
}