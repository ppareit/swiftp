package be.ppareit.swiftp.gui;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;
import android.widget.TextView;
import android.widget.Toast;

import net.vrallev.android.cat.Cat;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.ChrootPicker;

public class UserEditFragment extends Fragment {

    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;

    private FtpUser item;
    private OnEditFinishedListener editFinishedListener;
    private TextView chroot = null;
    private String uriString = "";
    private ChrootPicker chrootPicker = null;

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
        chrootPicker = new ChrootPicker();
        chroot = (TextView) root.findViewById(R.id.user_edit_chroot);
        chroot.setText(FsSettings.getDefaultChrootDir().getPath());
        chroot.setOnFocusChangeListener((v, hasFocus) -> {
            if (!hasFocus) return;
            chrootPicker.showFolderPicker(chroot.getText().toString(), null, getContext());
        });
        chroot.setOnClickListener(v -> {
            chrootPicker.showFolderPicker(chroot.getText().toString(), null, getContext());
        });
        chrootPicker.setOnTextEventListener(s -> chroot.setText(s));
        chrootPicker.setOnActionTreeEventListener(() -> {
            Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
            startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
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
            chrootPicker.save(this.getContext(), treeUri);
            uriString = treeUri.getPath();
        }
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
