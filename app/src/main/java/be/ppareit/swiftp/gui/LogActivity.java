package be.ppareit.swiftp.gui;

import android.os.Bundle;

import androidx.core.app.NavUtils;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.text.PrecomputedTextCompat;
import androidx.core.widget.TextViewCompat;

import android.os.Handler;
import android.os.Looper;
import android.view.Menu;
import android.view.MenuItem;
import android.view.ViewTreeObserver;
import android.widget.ScrollView;
import android.widget.TextView;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.utils.Logging;

public class LogActivity extends AppCompatActivity {

    private boolean isDisplayingLogcat = false;
    private Handler handler;
    private Runnable updateViewRunnable;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        setTheme(FsSettings.getTheme());
        super.onCreate(savedInstanceState);

        setContentView(R.layout.logs_layout);

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setHomeButtonEnabled(true);
            actionBar.setDisplayHomeAsUpEnabled(true);
        }

        final TextView tv = findViewById(R.id.logs_textview);
        final ScrollView scrollView = findViewById(R.id.logs_scrollview);

        // here we update the textview with the new message,
        // this updates every second, not nice
        // todo watch the files and only update on change
        handler = new Handler(Looper.getMainLooper());
        updateViewRunnable = new Runnable() {
            @Override
            public void run() {
                updateView(tv);
                handler.postDelayed(this, 1000); // Update every second
            }
        };
        handler.post(updateViewRunnable);

        ViewTreeObserver viewTreeObserver = tv.getViewTreeObserver();
        viewTreeObserver.addOnGlobalLayoutListener(() -> {
            scrollView.scrollTo(0, tv.getBottom());
        });

    }

    @Override
    protected void onDestroy() {
        handler.removeCallbacks(updateViewRunnable);
        super.onDestroy();
    }

    private void updateView(TextView tv) {

        if (isDisplayingLogcat) {
            showLogcat(tv);
        } else {
            showLog(tv);
        }

    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.logs_menu, menu);

        MenuItem displayLogcatItem = menu.findItem(R.id.action_display_logcat);
        MenuItem clearItem = menu.findItem(R.id.action_clear);

        displayLogcatItem.setChecked(isDisplayingLogcat);
        clearItem.setVisible(!isDisplayingLogcat);

        return true;
    }

    private String getLogcat() {
        StringBuilder log = new StringBuilder();
        try {
            Process process = Runtime.getRuntime().exec("logcat -d");
            BufferedReader bufferedReader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()));

            String line;
            while ((line = bufferedReader.readLine()) != null) {
                log.append(line).append("\n\n");
            }
        } catch (IOException e) { /**/ }
        return log.toString();
    }

    /*
     * Use this over setText() for a major speed improvement on long text
     * */
    private PrecomputedTextCompat getPrecomputedText(TextView v, String s) {
        PrecomputedTextCompat.Params params = TextViewCompat.getTextMetricsParams(v);
        return PrecomputedTextCompat.create(s, params);
    }

    private void showLogcat(TextView v) {
        new Thread(() -> {
            PrecomputedTextCompat ptc = getPrecomputedText(v, getLogcat());
            LogActivity.this.runOnUiThread(() -> {
                TextViewCompat.setPrecomputedText(v, ptc);
            });
        }).start();
    }

    private void showLog(TextView v) {
        Logging logging = new Logging();
        new Thread(() -> {
            PrecomputedTextCompat ptc = getPrecomputedText(v, logging.readLogFile());
            LogActivity.this.runOnUiThread(() -> {
                TextViewCompat.setPrecomputedText(v, ptc);
            });
        }).start();
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {

        int id = item.getItemId();
        TextView tv = findViewById(R.id.logs_textview);
        Logging logging = new Logging();

        if (id == android.R.id.home) {
            NavUtils.navigateUpFromSameTask(this);
            return true;
        } else if (id == R.id.action_clear) {
            if (tv.getText().toString().isEmpty())
                return true;
            logging.clearLog();
            return true;
        } else if (id == R.id.action_display_logcat) {
            item.setChecked(!item.isChecked());
            isDisplayingLogcat = item.isChecked();
            updateView(tv);
            invalidateOptionsMenu();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }
}