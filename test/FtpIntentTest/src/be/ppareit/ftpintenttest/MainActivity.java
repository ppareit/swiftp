package be.ppareit.ftpintenttest;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

/**
 * MainActivity for the FtpIntentTest application. This is part of Swiftp. This app is
 * used to test the different public intents for Swiftp. This application also serves as a
 * demonstration on how to use Swiftp from within an other application. For more
 * information, contact the developer by email.
 * 
 * @author pieter.pareit@gmail.com
 */
public class MainActivity extends Activity {

    // Swiftp: create constant strings for the intents
    static final String ACTION_START_FTPSERVER = "be.ppareit.swiftp.ACTION_START_FTPSERVER";
    static final String ACTION_STOP_FTPSERVER = "be.ppareit.swiftp.ACTION_STOP_FTPSERVER";
    static final String FTPSERVER_STARTED = "be.ppareit.swiftp.FTPSERVER_STARTED";
    static final String FTPSERVER_STOPPED = "be.ppareit.swiftp.FTPSERVER_STOPPED";

    TextView mStatusText = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        mStatusText = (TextView) findViewById(R.id.status_text);

        // Swiftp: register receiver to know if the ftp server has started or stopped
        IntentFilter intents = new IntentFilter();
        intents.addAction(FTPSERVER_STARTED);
        intents.addAction(FTPSERVER_STOPPED);
        registerReceiver(mStartStopReceiver, intents);

        TextView checkInstallText = (TextView) findViewById(R.id.installed_text);
        // Swiftp: check to see if Swiftp is installed
        if (appInstalled("be.ppareit.swiftp")) {
            checkInstallText.setText("The FTP Server application is installed");
        } else {
            checkInstallText.setText("No FTP Server, install this first");
        }

        Button startButton = (Button) findViewById(R.id.start_button);
        startButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // Swiftp: request ftp server to start
                Intent startIntent = new Intent(ACTION_START_FTPSERVER);
                sendBroadcast(startIntent);
            }
        });
        Button stopButton = (Button) findViewById(R.id.stop_button);
        startButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // Swiftp: request ftp server to stop
                Intent stopIntent = new Intent(ACTION_STOP_FTPSERVER);
                sendBroadcast(stopIntent);
            }
        });
    }

    @Override
    protected void onDestroy() {
        unregisterReceiver(mStartStopReceiver);
    };

    BroadcastReceiver mStartStopReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            // Swiftp: notify user if ftp server is running or not
            if (intent.getAction().equals(FTPSERVER_STARTED)) {
                mStatusText.setText("FTP Server is running");
            } else if (intent.getAction().equals(FTPSERVER_STOPPED)) {
                mStatusText.setText("FTP Server is down");
            }
        }
    };

    /**
     * Checks to see if apk is installed.
     * 
     * @param uri of the apk
     * @return true if the apk with uri is installed on system
     */
    private boolean appInstalled(String uri) {
        PackageManager pm = getPackageManager();
        boolean installed = false;
        try {
            pm.getPackageInfo(uri, PackageManager.GET_ACTIVITIES);
            installed = true;
        } catch (PackageManager.NameNotFoundException e) {
            installed = false;
        }
        return installed;
    }

}
