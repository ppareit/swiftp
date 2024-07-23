package be.ppareit.swiftp.utils;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;

import net.vrallev.android.cat.Cat;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;

public class Logging {

    private final SimpleDateFormat sdf = new SimpleDateFormat("dd:hh:mm:ss.SSS", Locale.getDefault());
    private boolean logging = true;

    public Logging() {
        initializeLogging();
    }

    public void initializeLogging() {
        logging = FsSettings.isLoggingEnabled();
        Cat.d("Logging enabled: " + logging);
    }

    public boolean isLoggingEnabled() {
        return logging;
    }

    private void saveLogging(String s) {
        new Thread(() -> {
            controlSize();
            try (FileOutputStream fos = App.getAppContext().openFileOutput("connLog",
                    Context.MODE_PRIVATE | Context.MODE_APPEND)) {
                fos.write(s.getBytes());
            } catch (FileNotFoundException e) {
                //
            } catch (IOException e) {
                //
            }
        }).start();
    }

    public void appendLog(String s) {
        if (isLoggingEnabled()) {
            saveLogging(sdf.format(new Date()) + ": " + s + '\n');
        }
    }

    public String readLogFile() {
        if (!isLoggingEnabled()) return "";
        try {
            FileInputStream fis = App.getAppContext().openFileInput("connLog");
            InputStreamReader inputStreamReader = new InputStreamReader(fis, StandardCharsets.UTF_8);
            StringBuilder stringBuilder = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(inputStreamReader)) {
                String line = reader.readLine();
                while (line != null) {
                    stringBuilder.append(line).append('\n');
                    line = reader.readLine();
                }
            } catch (IOException e) {
                // Error occurred when opening raw file for reading.
            }
            return stringBuilder.toString();
        } catch (Exception e) {
            //
        }
        return "";
    }

    public void clearLog() {
        if (!isLoggingEnabled()) return;
        new Thread(() -> {
            try (FileOutputStream fos = App.getAppContext().openFileOutput("connLog", Context.MODE_PRIVATE)) {
                fos.write("".getBytes());
            } catch (FileNotFoundException e) {
                //
            } catch (IOException e) {
                //
            }
        }).start();
    }

    /* // todo keep some amount of the old log?
    * Don't allow the log to keep growing or get too large.
    * */
    private void controlSize() {
        final File file = new File(App.getAppContext().getFilesDir(), "connLog");
        if (file.length() >= 100000) clearLog();
    }
}
