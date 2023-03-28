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
import android.util.Log;
import android.widget.ArrayAdapter;
import android.widget.ListView;


import java.io.File;
import java.io.IOException;

/**
 * Builder class for a folder picker dialog.
 */
public class FolderPickerDialogBuilder extends AlertDialog.Builder {

    private ArrayAdapter<String> mAdapter;
    private AlertDialog mAlert;

    private File mRoot;

    public FolderPickerDialogBuilder(Context context, File root) {
        super(context);
        mRoot = root;

        mAdapter = new ArrayAdapter<>(getContext(), android.R.layout.simple_list_item_1);

        update();

        ListView list = new ListView(getContext());
        list.setAdapter(mAdapter);
        list.setOnItemClickListener(
                (parentAdapterView, view, position, id) -> {
                    String dir = (String) parentAdapterView.getItemAtPosition(position);
                    final File parent;
                    if (dir.equals("..") && (parent = mRoot.getParentFile()) != null) {
                        mRoot = parent;
                    } else {
                        mRoot = new File(mRoot, dir);
                    }
                    update();
                }
        );

        setView(list);
    }

    @Override
    public AlertDialog create() {
        if (mAlert != null) throw new RuntimeException("Cannot reuse builder");
        mAlert = super.create();
        return mAlert;
    }

    private void update() {
        try {
            mRoot = new File(mRoot.getCanonicalPath());
        } catch (IOException e) {
            Log.w("swiftp","Directory root is incorrect, fixing to external storage.");
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
        if (dirs == null) {
            Log.w("swiftp", "Unable to receive dirs list, no Access rights?");
            Log.d("swiftp","Unable to fix, continue with empty list");
            dirs = new String[]{};
        }
        mAdapter.add("..");
        mAdapter.addAll(dirs);
    }

    public AlertDialog.Builder setSelectedButton(int textId, OnSelectedListener listener) {
        return setPositiveButton(textId,
                (dialog, which) -> listener.onSelected(mRoot.getAbsolutePath()));
    }

    public interface OnSelectedListener {
        void onSelected(String path);
    }

}
