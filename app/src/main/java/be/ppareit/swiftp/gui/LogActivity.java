package be.ppareit.swiftp.gui;


import android.animation.Animator;
import android.animation.ArgbEvaluator;
import android.animation.ObjectAnimator;
import android.animation.ValueAnimator;
import android.graphics.Color;
import android.os.Bundle;

import androidx.core.app.NavUtils;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.text.PrecomputedTextCompat;
import androidx.core.widget.TextViewCompat;

import android.view.MenuItem;
import android.widget.ImageView;
import android.widget.TextView;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.utils.Logging;

public class LogActivity extends AppCompatActivity {

    private ObjectAnimator anim = null;

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
        tv.setText("...");
        final ObjectAnimator loading = ObjectAnimator.ofObject(tv, "textColor",
                new ArgbEvaluator(), Color.BLACK, Color.LTGRAY).setDuration(100);
        loading.setRepeatCount(ValueAnimator.INFINITE);
        loading.start();

        //showLogcat(tv, loading);
        Logging logging = new Logging();
        showLog(tv, logging, loading);

        clear(tv, logging);
        refresh(tv, logging);
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

    private void showLogcat(TextView v, ObjectAnimator loading) {
        new Thread(() -> {
            PrecomputedTextCompat ptc = getPrecomputedText(v, getLogcat());
            LogActivity.this.runOnUiThread(() -> {
                TextViewCompat.setPrecomputedText(v, ptc);
                loading.end();
            });
        }).start();
    }

    private void showLog(TextView v, Logging logging, ObjectAnimator loading) {
        new Thread(() -> {
            PrecomputedTextCompat ptc = getPrecomputedText(v, logging.readLogFile());
            LogActivity.this.runOnUiThread(() -> {
                TextViewCompat.setPrecomputedText(v, ptc);
                if (loading != null) loading.end();
            });
        }).start();
    }

    private void refresh(TextView tv, Logging logging) {
        ImageView refresh = findViewById(R.id.logs_refresh_button);
        refresh.setOnClickListener(v -> {
            if (anim != null && anim.isRunning()) {
                anim.end();
                return;
            }
            anim = ObjectAnimator.ofFloat(v, "rotation", 0.0f, 360.0f);
            anim.setDuration(500);
            anim.addListener(new Animator.AnimatorListener() {
                @Override
                public void onAnimationStart(Animator animation) {
                }

                @Override
                public void onAnimationEnd(Animator animation) {
                    showLog(tv, logging, null);
                }

                @Override
                public void onAnimationCancel(Animator animation) {
                }

                @Override
                public void onAnimationRepeat(Animator animation) {
                }
            });
            anim.setStartDelay(500);
            anim.start();
        });
    }

    private void clear(TextView tv, Logging logging) {
        ImageView clear = findViewById(R.id.logs_clear_button);
        clear.setOnClickListener(v -> {
            if (tv.getText().toString().isEmpty()) return;
            logging.clearLog();
            tv.setText("");
        });
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