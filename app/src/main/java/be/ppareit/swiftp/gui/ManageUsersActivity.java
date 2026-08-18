// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.gui;

import android.os.Bundle;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.NavUtils;

import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

import be.ppareit.swiftp.R;

public class ManageUsersActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.manage_users_layout);
        setSupportActionBar(findViewById(R.id.my_toolbar));
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setTitle(R.string.manage_users_title);
        getSupportFragmentManager().beginTransaction()
                .replace(R.id.manage_users_activity_fragment, UserListFragment.newInstance())
                .commit();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.menu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            NavUtils.navigateUpFromSameTask(this);
            return true;
        }
        new be.ppareit.swiftp.gui.Menu().init(item, this);
        return true;
    }
}
