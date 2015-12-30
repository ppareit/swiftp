/*******************************************************************************
 * Copyright (c) 2012-2015 Pieter Pareit.
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * <p>
 * Contributors:
 * Pieter Pareit - initial API and implementation
 ******************************************************************************/

package be.ppareit.swiftp.gui;

import android.app.AlertDialog;
import android.content.Context;
import android.os.Environment;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.io.FilePermission;
import java.io.IOException;
import java.security.AccessController;

/**
 * Builder class for a folder picker dialog.
 */
public class FolderPickerDialogBuilder extends AlertDialog.Builder {

    private ArrayAdapter<String> mAdapter;
    private ListView mList;
    private AlertDialog mAlert;

    private File mRoot;

    public FolderPickerDialogBuilder(Context context, File root) {
        super(context);
        mRoot = root;

        mAdapter = new ArrayAdapter<>(getContext(), android.R.layout.simple_list_item_1);

        update();

        mList = new ListView(getContext());
        mList.setAdapter(mAdapter);
        mList.setOnItemClickListener(
                (parent, view, position, id) -> {
                    String dir = (String) parent.getItemAtPosition(position);
                    mRoot = new File(mRoot, dir);
                    update();
                }
        );

        setView(mList);
    }

    @Override
    public AlertDialog create() {
        if (mAlert != null) throw new RuntimeException("Cannot reuse builder");
        mAlert = super.create();
        return mAlert;
    }

    void update() {
        try {
            mRoot = new File(mRoot.getCanonicalPath());
        } catch (IOException e) {
            Cat.w("Directory root is incorrect, fixing to external storage.");
            mRoot = Environment.getExternalStorageDirectory();
        }

        if (mRoot.getAbsolutePath().equals("/")) {
            mRoot = Environment.getExternalStorageDirectory();
        }

        if (mAlert != null) {
            mAlert.setTitle(mRoot.getAbsolutePath());
        } else {
            setTitle(mRoot.getAbsolutePath());
        }

        mAdapter.clear();
        String[] dirs = mRoot.list(
                (dir, filename) -> {
                    File file = new File(dir, filename);
                    return (file.isDirectory() && !file.isHidden());
                });
        mAdapter.add("..");
        mAdapter.addAll(dirs);
    }

    AlertDialog.Builder setSelectedButton(int textId, OnSelectedListener listener) {
        return setPositiveButton(textId,
                (dialog, which) -> listener.onSelected(mRoot.getAbsolutePath()));
    }

    public interface OnSelectedListener {
        void onSelected(String path);
    }

}
