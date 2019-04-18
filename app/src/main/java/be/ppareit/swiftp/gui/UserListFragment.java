package be.ppareit.swiftp.gui;

import android.app.AlertDialog;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.Context;
import android.os.Bundle;
import android.os.Parcelable;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.server.FtpUser;

public class UserListFragment extends Fragment {

    private ListView listView;
    private Parcelable listViewState;

    public static UserListFragment newInstance() {
        return new UserListFragment();
    }

    @Nullable
    @Override
    public View onCreateView(LayoutInflater inflater, @Nullable ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.user_list_layout, container, false);
        listView = (ListView) root.findViewById(R.id.user_list);
        listView.setAdapter(new UserListAdapter(getActivity()));
        View addBtn = root.findViewById(R.id.user_add_btn);
        addBtn.setOnClickListener((buttonView) -> showEditItemFragment(null));
        return root;
    }

    @Override
    public void onPause() {
        super.onPause();
        listViewState = listView.onSaveInstanceState();
    }

    @Override
    public void onResume() {
        super.onResume();
        if (listViewState != null)
            listView.onRestoreInstanceState(listViewState);
    }

    private void showEditItemFragment(FtpUser item) {
        Fragment editFragment = UserEditFragment.newInstance(item, (oldItem, newItem) -> {
            if (oldItem != null) {
                FsSettings.modifyUser(oldItem.getUsername(), newItem);
            } else {
                try {
                    FsSettings.addUser(newItem);
                } catch (IllegalArgumentException ignored) {
                    Toast.makeText(getActivity(),
                            R.string.user_exists_error,
                            Toast.LENGTH_LONG).show();
                }
            }
            refreshUserList();
        });
        getActivity().getFragmentManager().beginTransaction()
                .replace(android.R.id.content, editFragment)
                .addToBackStack("default")
                .setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
                .commit();
    }

    private void showDeleteConfirmationDialog(FtpUser item) {
        AlertDialog dialog = new AlertDialog.Builder(getActivity())
                .setMessage(getString(R.string.confirm_delete_message, item.getUsername()))
                .setNegativeButton(android.R.string.no, null)
                .setPositiveButton(android.R.string.yes, (dialogInterface, whichButton) -> {
                    FsSettings.removeUser(item.getUsername());
                    refreshUserList();
                })
                .create();
        dialog.show();
    }

    private void refreshUserList() {
        UserListAdapter listAdapter = (UserListAdapter) listView.getAdapter();
        listAdapter.clear();
        listAdapter.addAll(FsSettings.getUsers());
        listAdapter.notifyDataSetInvalidated();
    }

    private class UserItemViewHolder {
        private TextView username, password, chroot;
        private View editBtn, deleteBtn;

        private void bind(View viewToHold) {
            username = (TextView) viewToHold.findViewById(R.id.user_name);
            password = (TextView) viewToHold.findViewById(R.id.user_password);
            chroot = (TextView) viewToHold.findViewById(R.id.user_chroot);
            editBtn = viewToHold.findViewById(R.id.user_edit_btn);
            deleteBtn = viewToHold.findViewById(R.id.user_delete_btn);
            viewToHold.setTag(this);
        }
    }

    private class UserListAdapter extends ArrayAdapter<FtpUser> {

        private LayoutInflater layoutInflater;

        private UserListAdapter(@NonNull Context context) {
            super(context, 0, FsSettings.getUsers());
            layoutInflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        }

        @NonNull
        @Override
        public View getView(final int position, @Nullable View convertView, @NonNull ViewGroup parent) {
            UserItemViewHolder viewHolder;
            if (convertView == null) {
                convertView = layoutInflater.inflate(R.layout.user_list_item_layout, parent, false);
                viewHolder = new UserItemViewHolder();
                viewHolder.bind(convertView);
            } else {
                viewHolder = (UserItemViewHolder) convertView.getTag();
            }
            final FtpUser item = getItem(position);
            viewHolder.username.setText(item.getUsername());
            viewHolder.password.setText(item.getPassword());
            viewHolder.chroot.setText(item.getChroot());
            viewHolder.editBtn.setOnClickListener((view) -> showEditItemFragment(item));
            viewHolder.deleteBtn.setOnClickListener((view) -> showDeleteConfirmationDialog(item));
            return convertView;
        }
    }
}
