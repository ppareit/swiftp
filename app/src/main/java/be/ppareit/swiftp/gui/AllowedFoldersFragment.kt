// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.gui

import android.app.Activity
import android.app.AlertDialog
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.provider.Settings
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.ArrayAdapter
import android.widget.Button
import android.widget.ListView
import android.widget.TextView
import android.widget.Toast

import androidx.cardview.widget.CardView
import androidx.fragment.app.Fragment

import com.google.android.material.floatingactionbutton.FloatingActionButton

import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.R
import be.ppareit.swiftp.Util
import be.ppareit.swiftp.users.UserStore
import be.ppareit.swiftp.utils.AllowedFolders
import be.ppareit.swiftp.utils.LegacyStoragePermission
import be.ppareit.swiftp.utils.StorageAccessMode
import be.ppareit.swiftp.utils.StorageAccessModeStore
import be.ppareit.swiftp.utils.StorageProbe
import be.ppareit.swiftp.utils.StorageTree

/**
 * Android keeps the persisted URI permissions and this screen shows them.
 */
class AllowedFoldersFragment : Fragment() {

    private lateinit var listView: ListView
    private lateinit var selectedHeading: TextView
    private lateinit var allFilesCard: CardView
    private lateinit var safCard: CardView
    private lateinit var allFilesDescription: TextView
    private lateinit var allFilesButton: Button
    private lateinit var safButton: Button
    private lateinit var addButton: FloatingActionButton
    private var waitingForSettingsGrant = false

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?,
    ): View {
        val root = inflater.inflate(R.layout.allowed_folders_list_layout, container, false)
        listView = root.findViewById(R.id.allowed_folders_list)
        selectedHeading = root.findViewById(R.id.allowed_folders_selected_heading)
        allFilesCard = root.findViewById(R.id.allowed_folders_all_files_card)
        safCard = root.findViewById(R.id.allowed_folders_saf_card)
        allFilesDescription = root.findViewById(R.id.allowed_folders_all_files_description)
        allFilesButton = root.findViewById(R.id.allowed_folders_all_files_btn)
        safButton = root.findViewById(R.id.allowed_folders_saf_btn)
        addButton = root.findViewById(R.id.allowed_folders_add_btn)

        waitingForSettingsGrant = savedInstanceState?.getBoolean(WAITING_FOR_SETTINGS) ?: false
        allFilesButton.setOnClickListener { askForFullAccess() }
        safButton.setOnClickListener { chooseSelectedFolders() }
        addButton.setOnClickListener { openPicker() }

        return root
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putBoolean(WAITING_FOR_SETTINGS, waitingForSettingsGrant)
        super.onSaveInstanceState(outState)
    }

    override fun onResume() {
        super.onResume()
        if (waitingForSettingsGrant) {
            waitingForSettingsGrant = false
            Util.resetScoped()
            if (LegacyStoragePermission.isGranted(requireContext())
                && StorageProbe.hasFullSdCardAccess()
            ) {
                activateAllFiles()
                return
            }
        }
        refresh()
    }

    private fun refresh() {
        var mode = StorageAccessModeStore.current()
        if (mode == StorageAccessMode.SELECTED_FOLDERS && !AllowedFolders.hasSavedFolders()) {
            StorageAccessModeStore.select(StorageAccessMode.UNCHOSEN)
            mode = StorageAccessMode.UNCHOSEN
        }

        val fullAccessWorks = StorageProbe.hasFullSdCardAccess()
        val safIsActive = mode == StorageAccessMode.SELECTED_FOLDERS
        val allFilesIsActive = mode == StorageAccessMode.ALL_FILES && fullAccessWorks
        setSelected(allFilesCard, mode == StorageAccessMode.ALL_FILES)
        setSelected(safCard, safIsActive)

        allFilesDescription.setText(
            if (mode == StorageAccessMode.ALL_FILES && !fullAccessWorks)
                R.string.allowed_folders_all_files_repair
            else R.string.allowed_folders_all_files_description
        )
        allFilesButton.setText(
            when {
                allFilesIsActive -> R.string.allowed_folders_in_use
                fullAccessWorks -> R.string.allowed_folders_use_all_files
                else -> R.string.allowed_folders_allow_permission
            }
        )
        allFilesButton.isEnabled = !allFilesIsActive

        safButton.setText(
            when {
                safIsActive -> R.string.allowed_folders_in_use
                AllowedFolders.hasSavedFolders() -> R.string.allowed_folders_use_selected
                else -> R.string.allowed_folders_choose_selected
            }
        )
        safButton.isEnabled = !safIsActive

        selectedHeading.visibility = if (safIsActive) View.VISIBLE else View.GONE
        listView.visibility = if (safIsActive) View.VISIBLE else View.GONE
        addButton.visibility = if (safIsActive) View.VISIBLE else View.GONE
        val folders = AllowedFolders.all()
        // A copy: ArrayAdapter keeps the list it is given and will mutate it, and this one
        // belongs to the cached AllowedFolders snapshot that the server reads from.
        listView.adapter =
            FolderAdapter(requireContext(), folders.toMutableList(), ::removeFolder)
    }

    private fun setSelected(card: CardView, selected: Boolean) {
        card.cardElevation = (if (selected) 8 else 2) * resources.displayMetrics.density
    }

    private fun chooseSelectedFolders() {
        if (AllowedFolders.hasSavedFolders()) {
            StorageAccessModeStore.select(StorageAccessMode.SELECTED_FOLDERS)
            FsService.restart()
            refresh()
            warnAboutStrandedUsers()
        } else {
            openPicker()
        }
    }

    /**
     * Modern Android opens its special-access screen. Older versions use the old storage
     * permission, which has to cope with a permanently denied dialog:
     *    - on a device that grants it, it serves the whole card
     *    - it has to cope with a permission that was denied, Android then
     *      shows no dialog at all and answers always denied, so the app
     *      info screen is the only place left to repair it.
     */
    private fun askForFullAccess() {
        if (StorageProbe.hasFullSdCardAccess()) {
            activateAllFiles()
        } else if (LegacyStoragePermission.usesSettingsGrant()) {
            waitingForSettingsGrant = true
            startActivity(LegacyStoragePermission.settingsIntent(requireContext()))
        } else if (androidWillNotAskAgain())
            openAppSettings() // is the only thing we can do and hope the users finds storage
        else
            requestPermissions(LegacyStoragePermission.PERMISSIONS, REQUEST_STORAGE_PERMISSION)
    }

    /**
     * Whether a tap would bring up no dialog at all, which is the state the app info screen is
     * for. Android has three states here and only two signals for them:
     *
     * asked before | shouldShowRequestPermissionRationale | what it means
     * -------------|--------------------------------------|---------------------------------------
     * no           | false                                | the dialog will appear
     * yes          | true                                 | denied once, it will appear again
     * yes          | false                                | denied for good, it will never appear
     *
     * In that last state requestPermissions shows nothing and answers denied
     */
    private fun androidWillNotAskAgain() = LegacyStoragePermission.wasRequested()
            && !shouldShowRequestPermissionRationale(LegacyStoragePermission.PERMISSIONS[0])

    @Deprecated("The rest of this app is on the same API; moving one screen would not help.")
    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray,
    ) {
        if (requestCode != REQUEST_STORAGE_PERMISSION) return
        if (LegacyStoragePermission.onResult(grantResults)) {
            activateAllFiles()
        } else if (grantResults.isNotEmpty() && androidWillNotAskAgain()) {
            Toast.makeText(
                requireContext(),
                R.string.allowed_folders_permission_blocked,
                Toast.LENGTH_LONG,
            ).show()
            openAppSettings()
        }
        refresh()
    }

    private fun activateAllFiles() {
        StorageAccessModeStore.select(StorageAccessMode.ALL_FILES)
        FsService.restart()
        refresh()
    }

    private fun openAppSettings() {
        waitingForSettingsGrant = true
        startActivity(
            Intent(
                Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
                Uri.fromParts("package", requireContext().packageName, null),
            )
        )
    }

    private fun openPicker() {
        startActivityForResult(Intent(Intent.ACTION_OPEN_DOCUMENT_TREE), ACTION_OPEN_DOCUMENT_TREE)
    }

    @Deprecated("The rest of this app is on the same API; moving one screen would not help.")
    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        super.onActivityResult(requestCode, resultCode, data)
        if (requestCode != ACTION_OPEN_DOCUMENT_TREE || resultCode != Activity.RESULT_OK) return
        val treeUri = data?.data ?: return
        if (!AllowedFolders.takeGrant(requireContext(), treeUri)) {
            Toast.makeText(
                requireContext(),
                R.string.allowed_folders_unsupported_provider,
                Toast.LENGTH_LONG,
            ).show()
            return
        }
        StorageAccessModeStore.select(StorageAccessMode.SELECTED_FOLDERS)
        // The storage mode may have just changed, so the running server has to be told.
        FsService.restart()
        refresh()
        warnAboutStrandedUsers()
    }

    private fun removeFolder(tree: StorageTree) {
        AllowedFolders.releaseGrant(requireContext(), tree)
        if (!AllowedFolders.hasSavedFolders()) {
            StorageAccessModeStore.select(StorageAccessMode.UNCHOSEN)
        }
        FsService.restart()
        Toast.makeText(
            requireContext(),
            getString(R.string.allowed_folders_removed, tree.name),
            Toast.LENGTH_SHORT,
        ).show()
        refresh()
        warnAboutStrandedUsers()
    }

    /**
     * Changing the allowed folders is what breaks a login that was kept in one of them, say
     * it here and offer the user to use all allowed folders.
     */
    private fun warnAboutStrandedUsers() {
        val stranded = UserStore.strandedUsers()
        if (stranded.isEmpty()) return
        val named = stranded.joinToString("\n") { "${it.username}: ${it.chroot}" }
        AlertDialog.Builder(requireContext())
            .setTitle(R.string.chroot_stranded_title)
            .setMessage(getString(R.string.chroot_stranded_message, named))
            .setNegativeButton(R.string.cancel, null)
            .setPositiveButton(R.string.chroot_stranded_fix) { _, _ ->
                stranded.forEach { UserStore.serveAllAllowedFolders(it.username) }
                FsService.checkUsersAvailable()
            }
            .show()
    }

    /** Not inner: ArrayAdapter has its own `remove(T)`, which silently shadowed the fragment's. */
    private class FolderAdapter(
        context: Context,
        folders: List<StorageTree>,
        private val onRemove: (StorageTree) -> Unit,
    ) : ArrayAdapter<StorageTree>(context, R.layout.allowed_folders_item_layout, folders) {

        override fun getView(position: Int, convertView: View?, parent: ViewGroup): View {
            val view = convertView ?: LayoutInflater.from(context)
                .inflate(R.layout.allowed_folders_item_layout, parent, false)
            val tree = getItem(position) ?: return view

            view.findViewById<TextView>(R.id.allowed_folder_name).text = tree.name
            view.findViewById<TextView>(R.id.allowed_folder_path).text = tree.rootPath
            view.findViewById<Button>(R.id.allowed_folder_remove_btn)
                .setOnClickListener { onRemove(tree) }
            return view
        }
    }

    companion object {
        private const val ACTION_OPEN_DOCUMENT_TREE = 94
        private const val REQUEST_STORAGE_PERMISSION = 95
        private const val WAITING_FOR_SETTINGS = "waiting_for_settings_grant"
    }
}
