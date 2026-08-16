package be.ppareit.swiftp.gui;

import android.app.AlertDialog;
import android.graphics.Rect;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.view.inputmethod.EditorInfo;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.Toast;

import com.google.android.material.floatingactionbutton.FloatingActionButton;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.server.FtpUser;
import be.ppareit.swiftp.utils.ChrootPicker;

public class UserListFragment extends Fragment {

    /** The keyboard covers a good third of the screen, the system bars stay well under this. */
    private static final float KEYBOARD_SCREEN_FRACTION = 0.15f;

    private ScrollView scrollView;
    private LinearLayout listView;
    private FloatingActionButton addBtn;
    private ViewTreeObserver.OnGlobalLayoutListener keyboardWatcher;

    public static UserListFragment newInstance() {
        return new UserListFragment();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.user_list_layout, container, false);
        scrollView = root.findViewById(R.id.user_list_scroll);
        listView = root.findViewById(R.id.user_list);
        addBtn = root.findViewById(R.id.user_add_btn);
        addBtn.setOnClickListener((buttonView) -> addUser());
        keyboardWatcher = () -> updateAddButton(root);
        root.getViewTreeObserver().addOnGlobalLayoutListener(keyboardWatcher);
        refreshUserList();
        return root;
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        View root = getView();
        if (root != null) root.getViewTreeObserver().removeOnGlobalLayoutListener(keyboardWatcher);
    }

    /**
     * Dont show the Add FAB while keyboard is visible
     */
    private void updateAddButton(View root) {
        // we measure, windows insets only tell keyboard visible starting API 30
        final Rect visible = new Rect();
        root.getWindowVisibleDisplayFrame(visible);
        final int windowHeight = root.getRootView().getHeight();
        if (windowHeight <= 0) return;
        final boolean keyboardVisible =
                windowHeight - visible.height() > windowHeight * KEYBOARD_SCREEN_FRACTION;
        if (keyboardVisible) {
            addBtn.hide();
        } else {
            addBtn.show();
        }
    }

    @Override
    public void onPause() {
        super.onPause();
        commitFocusedField();
    }

    /** Leaving the screen has to store what is being typed, there is no save button to press. */
    private void commitFocusedField() {
        View focused = listView.findFocus();
        if (focused != null) focused.clearFocus();
    }

    private void addUser() {
        commitFocusedField();
        FtpUser user = new FtpUser(freeUsername(),
                getString(R.string.password_default),
                FsSettings.getDefaultChrootDir().getPath(), "");
        FsSettings.addUser(user);
        refreshUserList();
        scrollView.post(() -> scrollView.fullScroll(View.FOCUS_DOWN));
    }

    /** A new user starts out with a name that is not taken yet, so it can be stored right away. */
    private String freeUsername() {
        String username = "ftp";
        for (int i = 2; FsSettings.getUser(username) != null; i++) {
            username = "ftp" + i;
        }
        return username;
    }

    private void showDeleteConfirmationDialog(FtpUser item) {
        AlertDialog dialog = new AlertDialog.Builder(getActivity())
                .setMessage(getString(R.string.confirm_delete_message, item.getUsername()))
                .setNegativeButton(android.R.string.no, null)
                .setPositiveButton(android.R.string.yes, (dialogInterface, whichButton) -> {
                    FsSettings.removeUser(item.getUsername(), true);
                    refreshUserList();
                })
                .create();
        dialog.show();
    }

    private void refreshUserList() {
        final LayoutInflater inflater = LayoutInflater.from(getActivity());
        listView.removeAllViews();
        for (FtpUser user : FsSettings.getUsers()) {
            View row = inflater.inflate(R.layout.user_list_item_layout, listView, false);
            new UserItemViewHolder(row).show(user);
            listView.addView(row);
        }
    }

    private void showToast(int errorResId) {
        Toast.makeText(getActivity(), errorResId, Toast.LENGTH_LONG).show();
    }

    private class UserItemViewHolder {
        private final EditText username, password;
        private final TextView chroot;
        private FtpUser item;

        private UserItemViewHolder(View row) {
            username = row.findViewById(R.id.user_name);
            password = row.findViewById(R.id.user_password);
            chroot = row.findViewById(R.id.user_chroot);

            username.setOnFocusChangeListener((v, hasFocus) -> {
                if (!hasFocus) commit();
            });
            password.setOnFocusChangeListener((v, hasFocus) -> {
                if (!hasFocus) commit();
            });
            password.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId != EditorInfo.IME_ACTION_DONE) return false;
                password.clearFocus();
                return false;
            });
            chroot.setOnClickListener(v -> pickChroot());
            row.findViewById(R.id.user_delete_btn)
                    .setOnClickListener(v -> showDeleteConfirmationDialog(item));
        }

        private void show(FtpUser user) {
            item = user;
            username.setText(user.getUsername());
            password.setText(user.getPassword());
            chroot.setText(user.getChroot());
        }

        private void pickChroot() {
            ChrootPicker picker = new ChrootPicker();
            picker.setOnTextEventListener(path -> {
                chroot.setText(path);
                commit();
            });
            picker.showFolderPicker(chroot.getText().toString(), null, getContext());
        }

        /**
         * Stores what the row shows. A refused edit resets what is stored.
         */
        private void commit() {
            final String newUsername = username.getText().toString();
            final String newPassword = password.getText().toString();
            final String newChroot = chroot.getText().toString();
            if (newUsername.equals(item.getUsername())
                    && newPassword.equals(item.getPassword())
                    && newChroot.equals(item.getChroot())) {
                return;
            }
            if (!newUsername.matches("[a-zA-Z0-9]+")) {
                showToast(R.string.username_validation_error);
                show(item);
                return;
            }
            if (!newPassword.matches("[a-zA-Z0-9]+")) {
                showToast(R.string.password_validation_error);
                show(item);
                return;
            }
            if (!newUsername.equals(item.getUsername()) && FsSettings.getUser(newUsername) != null) {
                showToast(R.string.user_exists_error);
                show(item);
                return;
            }
            // the allowed folders are app-wide, so a user is only a name, a password and a
            // chroot in one of the allowed folders
            FtpUser newItem = new FtpUser(newUsername, newPassword, newChroot, "");
            FsSettings.modifyUser(item.getUsername(), newItem);
            // the constructor refuses a chroot that is not a directory, so show what was stored
            show(newItem);
        }
    }
}
