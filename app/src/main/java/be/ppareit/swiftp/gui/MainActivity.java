/*******************************************************************************
 * Copyright (c) 2012-2013 Pieter Pareit.
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
import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.os.Build.VERSION;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.pm.ShortcutInfoCompat;
import androidx.core.content.pm.ShortcutManagerCompat;
import androidx.core.graphics.drawable.IconCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;

import net.vrallev.android.cat.Cat;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.BuildConfig;
import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.R;

/**
 * This is the main activity for swiftp, it enables the user to start the server service
 * and allows the users to change the settings.
 */
public class MainActivity extends AppCompatActivity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Cat.d("created");
        super.onCreate(savedInstanceState);

        setContentView(R.layout.main_layout);
        setSupportActionBar(findViewById(R.id.my_toolbar));
        syncUpArrowWithBackStack();

        if (App.isFreeVersion() && App.isPaidVersionInstalled()) {
            Cat.d("Running demo while paid is installed");
            AlertDialog ad = new AlertDialog.Builder(this)
                    .setTitle(R.string.demo_while_paid_dialog_title)
                    .setMessage(R.string.demo_while_paid_dialog_message)
                    .setPositiveButton(getText(android.R.string.ok), (d, w) -> finish())
                    .create();
            ad.show();
        }

        getSupportFragmentManager().beginTransaction()
                .replace(R.id.main_activity_fragment, new PreferenceFragment(), null)
                .commit();

        if (VERSION.SDK_INT >= 25) {
            Intent intent = new Intent("Intent.QuickOn", null, MainActivity.this, MainActivity.class);
            ShortcutInfoCompat shortcut = new ShortcutInfoCompat.Builder(getApplicationContext(), "id1forever")
                    .setShortLabel(getString(R.string.shortcut_on))
                    .setIcon(IconCompat.createWithResource(getApplicationContext(), R.drawable.widget_on))
                    .setIntent(intent)
                    .build();
            ShortcutManagerCompat.pushDynamicShortcut(getApplicationContext(), shortcut);
            if (getIntent().getAction() != null && getIntent().getAction().equals("Intent.QuickOn")) {
                FsService.start();
            }
        }
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
            getSupportFragmentManager().popBackStack();
            return true;
        }
        new be.ppareit.swiftp.gui.Menu().init(item, this);
        return true;
    }

    private void syncUpArrowWithBackStack() {
        getSupportFragmentManager().registerFragmentLifecycleCallbacks(
                new FragmentManager.FragmentLifecycleCallbacks() {
                    @Override
                    public void onFragmentResumed(@NonNull FragmentManager fm, @NonNull Fragment f) {
                        showUpArrow(f.getTag() != null);
                    }
                }, false);
        showUpArrow(false);
    }

    private void showUpArrow(boolean show) {
        if (getSupportActionBar() == null) return;
        getSupportActionBar().setDisplayHomeAsUpEnabled(show);
    }
}
