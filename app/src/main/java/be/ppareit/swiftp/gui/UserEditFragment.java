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

    private FtpUser item;
    private OnEditFinishedListener editFinishedListener;
    private TextView chroot = null;
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
                // the allowed folders are app-wide, so a user is only a name, a password and a
                // chroot in one of the allowed folders
                editFinishedListener.onEditActionFinished(item,
                        new FtpUser(newUsername, newPassword, newChroot, ""));
                goBack();
            }
        });
        root.findViewById(R.id.user_cancel_btn).setOnClickListener((buttonView) -> goBack());
        return root;
    }

    private void goBack() {
        requireActivity().getOnBackPressedDispatcher().onBackPressed();
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
