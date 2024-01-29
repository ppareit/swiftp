package be.ppareit.swiftp.gui;


import android.app.Activity;
import android.app.AlertDialog;
import android.app.Fragment;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.documentfile.provider.DocumentFile;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import net.vrallev.android.cat.Cat;

import java.io.File;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.FileUtil;

public class UserEditFragment extends Fragment {

    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;

    private FtpUser item;
    private OnEditFinishedListener editFinishedListener;
    private boolean isShowingFolderPicker = false;
    private TextView chroot = null;
    private String uriString = "";

    public static UserEditFragment newInstance(@Nullable FtpUser item, @NonNull OnEditFinishedListener listener) {
        UserEditFragment fragment = new UserEditFragment();
        fragment.editFinishedListener = listener;
        if (item != null) {
            fragment.item = item;
        }
        return fragment;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.user_edit_layout, container, false);
        EditText username = (EditText) root.findViewById(R.id.user_edit_name);
        EditText password = (EditText) root.findViewById(R.id.user_edit_password);
        chroot = (TextView) root.findViewById(R.id.user_edit_chroot);
        chroot.setText(FsSettings.getDefaultChrootDir().getPath());
        chroot.setOnFocusChangeListener((v, hasFocus) -> {
            if (!hasFocus) return;
            showFolderPicker(chroot);
        });
        chroot.setOnClickListener(v -> {
            showFolderPicker(chroot);
        });

        if (item != null) {
            username.setText(item.getUsername());
            password.setText(item.getPassword());
            chroot.setText(item.getChroot());
        }

        root.findViewById(R.id.user_save_btn).setOnClickListener((buttonView) -> {
            String newUsername = username.getText().toString();
            String newPassword = password.getText().toString();
            String newChroot = chroot.getText().toString();
            if (validateInput(newUsername, newPassword)) {
                editFinishedListener.onEditActionFinished(item, new FtpUser(newUsername, newPassword, newChroot, uriString));
                getActivity().onBackPressed();
            }
        });
        root.findViewById(R.id.user_cancel_btn).setOnClickListener((buttonView) -> getActivity().onBackPressed());
        return root;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent resultData) {
        Cat.d("onActivityResult called");
        if (requestCode == ACTION_OPEN_DOCUMENT_TREE && resultCode == Activity.RESULT_OK) {
            if (resultData == null) return;
            Uri treeUri = resultData.getData();
            if (treeUri == null) return;
            String path = treeUri.getPath();
            Cat.d("Action Open Document Tree on path " + path);
            // *************************************
            // The order following here is critical. They must stay ordered as they are.
            setPermissionToUseExternalStorage(treeUri);
            scopedStorageChrootOverride(treeUri);
            uriString = treeUri.getPath();
        }
    }

    private void setPermissionToUseExternalStorage(Uri treeUri) {
            try {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                        FsSettings.setExternalStorageUri(treeUri.toString());
                        getActivity().getContentResolver()
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
            if (treePath.contains("primary:")) treePath = treePath.substring(treePath.indexOf(":") + 1);
            else if (treePath.contains(":")) {
                newPath = "/storage/";
                treePath = treePath.replace("/tree/", "");
                treePath = treePath.replace(":", "/");
            }
            newPath += treePath;
            if (chroot != null) chroot.setText(newPath);
        }
    }

    private void showFolderPicker(TextView chrootView) {
        if (Util.useScopedStorage()) {
            Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
            startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
            return;
        }
        if (isShowingFolderPicker)
            return;
        isShowingFolderPicker = true;
        final File startDir;
        if (chrootView.getText().toString().isEmpty()) {
            startDir = Environment.getExternalStorageDirectory();
        } else {
            startDir = new File((chrootView.getText().toString()));
        }
        AlertDialog folderPicker = new FolderPickerDialogBuilder(getActivity(), startDir)
                .setSelectedButton(R.string.select, path -> {
                    final File root = new File(path);
                    if (!root.canRead()) {
                        showToast(R.string.notice_cant_read_write);
                    } else if (!root.canWrite()) {
                        showToast(R.string.notice_cant_write);
                    }
                    chrootView.setText(path);
                })
                .setNegativeButton(R.string.cancel, null)
                .create();
        folderPicker.setOnDismissListener(dialog -> isShowingFolderPicker = false);
        folderPicker.show();
    }

    private boolean validateInput(String username, String password) {
        if (!username.matches("[a-zA-Z0-9]+")) {
            showToast(R.string.username_validation_error);
            return false;
        }
        if (!password.matches("[a-zA-Z0-9]+")) {
            showToast(R.string.password_validation_error);
            return false;
        }
        return true;
    }

    private void showToast(int errorResId) {
        Toast.makeText(getActivity(), errorResId, Toast.LENGTH_LONG).show();
    }

    interface OnEditFinishedListener {
        void onEditActionFinished(FtpUser oldItem, FtpUser newItem);
    }
}
