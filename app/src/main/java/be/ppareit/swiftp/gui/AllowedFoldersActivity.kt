package be.ppareit.swiftp.gui

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem

import androidx.appcompat.app.AppCompatActivity
import androidx.core.app.NavUtils

import be.ppareit.swiftp.FsSettings
import be.ppareit.swiftp.R

/**
 * Hosts the list of folders the user has allowed SwiFTP to serve.
 */
class AllowedFoldersActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        setTheme(FsSettings.getTheme())
        super.onCreate(savedInstanceState)
        setContentView(R.layout.allowed_folders_layout)
        setSupportActionBar(findViewById(R.id.my_toolbar))
        supportActionBar?.setDisplayHomeAsUpEnabled(true)
        setTitle(R.string.allowed_folders_title)
        supportFragmentManager.beginTransaction()
            .replace(R.id.allowed_folders_activity_fragment, AllowedFoldersFragment())
            .commit()
    }

    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        menuInflater.inflate(R.menu.menu, menu)
        return true
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean {
        if (item.itemId == android.R.id.home) {
            NavUtils.navigateUpFromSameTask(this)
            return true
        }
        Menu().init(item, this)
        return true
    }
}
