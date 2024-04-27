package be.ppareit.swiftp.gui;


import android.animation.ArgbEvaluator;
import android.animation.ObjectAnimator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.net.Uri;
import android.os.Bundle;

import androidx.core.app.NavUtils;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;

import android.preference.PreferenceManager;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;

import net.vrallev.android.cat.Cat;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.R;
import be.ppareit.swiftp.utils.ChrootPicker;

public class ManageAnonActivity extends AppCompatActivity {

    private static final int ACTION_OPEN_DOCUMENT_TREE = 42;
    ChrootPicker chrootPicker = null;

    @SuppressLint("ClickableViewAccessibility")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        setTheme(FsSettings.getTheme());
        super.onCreate(savedInstanceState);

        setContentView(R.layout.anon_layout);

        ActionBar actionBar = getSupportActionBar();
        if (actionBar != null) {
            actionBar.setHomeButtonEnabled(true);
            actionBar.setDisplayHomeAsUpEnabled(true);
        }

        chrootPicker = new ChrootPicker();
        TextView chroot = findViewById(R.id.anon_chroot);
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
        String chrootString = sp.getString("anonChroot", "");
        chroot.setText(chrootString);
        chroot.setOnTouchListener((v, event) -> {
            sp.edit().remove("anonChroot").apply();
            sp.edit().remove("anonUriString").apply();
            if (event.getAction() == MotionEvent.ACTION_DOWN) {
                chrootPicker.showFolderPicker(chroot.getText().toString(), this, null);
            }
            return true;
        });
        chrootPicker.setOnTextEventListener(s -> {
            chroot.setText(s);
            sp.edit().putString("anonChroot", s).apply();
        });
        chrootPicker.setOnActionTreeEventListener(() -> {
            Intent intent = new Intent(Intent.ACTION_OPEN_DOCUMENT_TREE);
            startActivityForResult(intent, ACTION_OPEN_DOCUMENT_TREE);
        });

        EditText anonMaxCon = findViewById(R.id.anon_max);
        anonMaxCon.setText(String.valueOf(FsSettings.getAnonMaxConNumber()));
        anonMaxCon.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void afterTextChanged(Editable s) {
                sp.edit().putString("anon_max", s.toString()).apply();
            }
        });

        CheckBox anonCB = findViewById(R.id.anon_enable);
        anonCB.setChecked(sp.getBoolean("allow_anonymous", false));
        anonCB.setOnCheckedChangeListener((buttonView, isChecked) -> {
            SharedPreferences sp1 = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
            if (isChecked ) {
                if (sp.getString("anonChroot", "").isEmpty()) {
                    // Deny as workaround to problems like app crashing with scoped storage.
                    anonCB.setChecked(false);
                    final ObjectAnimator loading = ObjectAnimator.ofObject((TextView) chroot, "backgroundColor",
                            new ArgbEvaluator(), Color.RED, Color.TRANSPARENT).setDuration(1000);
                    loading.start();
                    return;
                }
                sp1.edit().putBoolean("allow_anonymous", true).apply();
            }
            else sp1.edit().putBoolean("allow_anonymous", false).apply();
        });
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent resultData) {
        super.onActivityResult(requestCode, resultCode, resultData);
        Cat.d("onActivityResult called");
        if (requestCode == ACTION_OPEN_DOCUMENT_TREE && resultCode == Activity.RESULT_OK) {
            if (resultData == null) return;
            Uri treeUri = resultData.getData();
            if (treeUri == null) return;
            String path = treeUri.getPath();
            Cat.d("Action Open Document Tree on path " + path);
            chrootPicker.save(this.getApplicationContext(), treeUri);
            String uriString = treeUri.getPath();
            SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(App.getAppContext());
            sp.edit().putString("anonUriString", uriString).apply();
        }
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