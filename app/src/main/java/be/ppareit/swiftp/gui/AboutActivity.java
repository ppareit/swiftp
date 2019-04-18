package be.ppareit.swiftp.gui;


import android.os.Build;
import android.os.Bundle;
import androidx.core.app.NavUtils;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import android.view.MenuItem;
import android.widget.TextView;

import be.ppareit.swiftp.BuildConfig;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;

/**
 * Created by ppareit on 5/02/17.
 */

public class AboutActivity extends AppCompatActivity {


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        setTheme(FsSettings.getTheme());
        super.onCreate(savedInstanceState);

        setContentView(R.layout.about_layout);

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setHomeButtonEnabled(true);
            actionBar.setDisplayHomeAsUpEnabled(true);
        }

        TextView packageNameText = (TextView) findViewById(R.id.about_package_name);
        packageNameText.setText(BuildConfig.APPLICATION_ID + " (" + BuildConfig.FLAVOR + ")");

        TextView versionInfoText = (TextView) findViewById(R.id.about_version_info);
        versionInfoText.setText(BuildConfig.VERSION_NAME + " - " + BuildConfig.VERSION_CODE + " (" + Build.VERSION.RELEASE + "-" + Build.VERSION.SDK_INT + ")");
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            NavUtils.navigateUpFromSameTask(this);
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
