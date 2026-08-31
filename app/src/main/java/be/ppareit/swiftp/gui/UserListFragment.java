// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.gui;

import android.app.AlertDialog;
import android.graphics.Rect;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.SwitchCompat;
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

import java.io.File;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.users.FtpUser;
import be.ppareit.swiftp.users.UserStore;
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
                FsSettings.getDefaultChrootDir().getPath());
        UserStore.INSTANCE.add(user);
        refreshUserList();
        scrollView.post(() -> scrollView.fullScroll(View.FOCUS_DOWN));
    }

    /** A new user starts out with a name that is not taken yet, so it can be stored right away. */
    private String freeUsername() {
        String username = "ftp";
        for (int i = 2; UserStore.INSTANCE.user(username) != null; i++) {
            username = "ftp" + i;
        }
        return username;
    }

    private void showDeleteConfirmationDialog(FtpUser item) {
        AlertDialog dialog = new AlertDialog.Builder(getActivity())
                .setMessage(getString(R.string.confirm_delete_message, item.getUsername()))
                .setNegativeButton(android.R.string.no, null)
                .setPositiveButton(android.R.string.yes, (dialogInterface, whichButton) -> {
                    UserStore.INSTANCE.remove(item.getUsername());
                    refreshUserList();
                })
                .create();
        dialog.show();
    }

    private void refreshUserList() {
        final LayoutInflater inflater = LayoutInflater.from(getActivity());
        listView.removeAllViews();
        // anonymous is not one of the users, it heads the list as its own card
        View anonRow = inflater.inflate(R.layout.anon_list_item_layout, listView, false);
        new AnonItemViewHolder(anonRow).show();
        listView.addView(anonRow);
        for (FtpUser user : UserStore.INSTANCE.users()) {
            View row = inflater.inflate(R.layout.user_list_item_layout, listView, false);
            new UserItemViewHolder(row).show(user);
            listView.addView(row);
        }
    }

    private void showToast(int errorResId) {
        Toast.makeText(getActivity(), errorResId, Toast.LENGTH_LONG).show();
    }

    /**
     * The anonymous login is a view on three preferences, not on a stored user: the name is
     * fixed by the protocol, the password is ignored and the access is read only.
     */
    private class AnonItemViewHolder {
        private final SwitchCompat enable;
        private final View details;
        private final TextView chroot;
        private final EditText maxConnections;

        private AnonItemViewHolder(View row) {
            enable = row.findViewById(R.id.anon_enable);
            details = row.findViewById(R.id.anon_details);
            chroot = row.findViewById(R.id.anon_chroot);
            maxConnections = row.findViewById(R.id.anon_max);

            enable.setOnClickListener(v -> toggle(enable.isChecked()));
            chroot.setOnClickListener(v -> pickChroot());
            maxConnections.setOnFocusChangeListener((v, hasFocus) -> {
                if (!hasFocus) commitMaxConnections();
            });
            maxConnections.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId != EditorInfo.IME_ACTION_DONE) return false;
                maxConnections.clearFocus();
                return false;
            });
        }

        private void show() {
            final boolean allowed = FsSettings.allowAnonymous();
            chroot.setText(FsSettings.getAnonChroot());
            maxConnections.setText(String.valueOf(FsSettings.getAnonMaxConNumber()));
            enable.setChecked(allowed);
            details.setVisibility(allowed ? View.VISIBLE : View.GONE);
        }

        private void toggle(boolean allowed) {
            if (allowed && !new File(chroot.getText().toString()).isDirectory()) {
                // logging in on a folder that is not there fails, so start out on one that is
                setChroot(FsSettings.getDefaultChrootDir().getPath());
            }
            FsSettings.setAllowAnonymous(allowed);
            details.setVisibility(allowed ? View.VISIBLE : View.GONE);
        }

        private void pickChroot() {
            ChrootPicker picker = new ChrootPicker();
            picker.setOnTextEventListener(this::setChroot);
            picker.showFolderPicker(chroot.getText().toString(), null, getContext());
        }

        private void setChroot(String path) {
            chroot.setText(path);
            FsSettings.setAnonChroot(path);
        }

        /** Stores what the field shows, a refused edit resets what is stored. */
        private void commitMaxConnections() {
            int max;
            try {
                max = Integer.parseInt(maxConnections.getText().toString());
            } catch (NumberFormatException e) {
                max = 0;
            }
            // an empty or zero limit would refuse every anonymous login
            if (max < 1) max = 1;
            FsSettings.setAnonMaxConNumber(max);
            maxConnections.setText(String.valueOf(max));
        }
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
            if (newUsername.equalsIgnoreCase("anonymous")) {
                // the name is taken by the card at the top of the list
                showToast(R.string.username_anonymous_error);
                show(item);
                return;
            }
            if (!newUsername.equals(item.getUsername()) && UserStore.INSTANCE.user(newUsername) != null) {
                showToast(R.string.user_exists_error);
                show(item);
                return;
            }
            // the allowed folders are app-wide, so a user is only a name, a password and a
            // chroot in one of the allowed folders
            FtpUser newItem = new FtpUser(newUsername, newPassword, newChroot);
            // the store reads back a chroot that is not a directory as the default, so show
            // what it holds rather than what was typed
            show(UserStore.INSTANCE.modify(item.getUsername(), newItem));
        }
    }
}
