package be.ppareit.swiftp.gui;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.os.Build;
import android.view.MenuItem;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import be.ppareit.swiftp.BuildConfig;
import be.ppareit.swiftp.R;

public class Menu {

    public void init(MenuItem item, AppCompatActivity activity) {
        if (item == null) return;
        if (item.getItemId() == R.id.action_feedback) {
            String to = "pieter.pareit@gmail.com";
            String subject = "FTP Server feedback";
            String message = "Device: " + Build.MODEL + "\n" +
                    "Android version: " + Build.VERSION.RELEASE + "-" + Build.VERSION.SDK_INT + "\n" +
                    "Application: " + BuildConfig.APPLICATION_ID + " (" + BuildConfig.FLAVOR + ")\n" +
                    "Application version: " + BuildConfig.VERSION_NAME + " - " + BuildConfig.VERSION_CODE + "\n" +
                    "Feedback: \n_";

            Intent emailIntent = new Intent(Intent.ACTION_SEND);
            emailIntent.putExtra(Intent.EXTRA_EMAIL, new String[]{to});
            emailIntent.putExtra(Intent.EXTRA_SUBJECT, subject);
            emailIntent.putExtra(Intent.EXTRA_TEXT, message);
            emailIntent.setType("message/rfc822");

            try {
                activity.startActivity(emailIntent);
                Toast.makeText(activity, R.string.use_english, Toast.LENGTH_LONG).show();
            } catch (ActivityNotFoundException exception) {
                Toast.makeText(activity, R.string.unable_to_start_mail_client, Toast.LENGTH_LONG).show();
            }

        } else if (item.getItemId() == R.id.action_about) {
            activity.startActivity(new Intent(activity, AboutActivity.class));
        }
    }
}
