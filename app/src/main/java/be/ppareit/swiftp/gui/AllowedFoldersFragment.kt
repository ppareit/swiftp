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

import androidx.fragment.app.Fragment

import com.google.android.material.floatingactionbutton.FloatingActionButton

import be.ppareit.swiftp.FsService
import be.ppareit.swiftp.R
import be.ppareit.swiftp.utils.AllowedFolders
import be.ppareit.swiftp.utils.LegacyStoragePermission
import be.ppareit.swiftp.utils.StorageProbe
import be.ppareit.swiftp.utils.StorageTree

/**
 * Android keeps the persisted URI permissions and this screen shows them.
 */
class AllowedFoldersFragment : Fragment() {

    private lateinit var listView: ListView
    private lateinit var explanation: TextView
    private lateinit var permissionButton: Button

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?,
    ): View {
        val root = inflater.inflate(R.layout.allowed_folders_list_layout, container, false)
        listView = root.findViewById(R.id.allowed_folders_list)
        explanation = root.findViewById(R.id.allowed_folders_explanation)
        permissionButton = root.findViewById(R.id.allowed_folders_permission_btn)
        permissionButton.setOnClickListener { askForFullAccess() }

        root.findViewById<FloatingActionButton>(R.id.allowed_folders_add_btn)
            .setOnClickListener { pickFolder() }

        return root
    }

    override fun onResume() {
        super.onResume()
        refresh()
    }

    private fun refresh() {
        val folders = AllowedFolders.all()
        val servesTheCard = StorageProbe.hasFullSdCardAccess()
        val canAskForFullAccess = canAskForFullAccess()
        explanation.setText(
            when {
                folders.isNotEmpty() -> R.string.allowed_folders_some_explanation
                servesTheCard -> R.string.allowed_folders_full_sdcard_explanation
                canAskForFullAccess -> R.string.allowed_folders_permission_explanation
                else -> R.string.allowed_folders_none_explanation
            }
        )
        permissionButton.visibility = if (canAskForFullAccess) View.VISIBLE else View.GONE
        // A copy: ArrayAdapter keeps the list it is given and will mutate it, and this one
        // belongs to the cached AllowedFolders snapshot that the server reads from.
        listView.adapter =
            FolderAdapter(requireContext(), folders.toMutableList(), ::removeFolder)
    }

    /**
     * Whether the whole card is one grant away
     */
    private fun canAskForFullAccess() = AllowedFolders.isEmpty()
            && !StorageProbe.hasFullSdCardAccess()
            && LegacyStoragePermission.isMissing(requireContext())

    /**
     * On a device that can serve everything, the *first* folder is the one that switches the app
     * to SAF, and from then on only the chosen folders are served. So the 'Add Folder' fab is
     * limiting the allowed folders, and it says so before the picker opens.
     *
     * A device that only *could* serve everything, because the permission is there for the
     * asking, has the same choice to make: picking a folder here settles it for SAF and leaves
     * the simpler route unused. The warning differs only in that the card is not served yet.
     */
    private fun pickFolder() {
        val servesTheCard = AllowedFolders.isEmpty() && StorageProbe.hasFullSdCardAccess()
        if (!servesTheCard && !canAskForFullAccess()) {
            openPicker()
            return
        }
        AlertDialog.Builder(requireContext())
            .setTitle(R.string.allowed_folders_limit_title)
            .setMessage(
                if (servesTheCard) R.string.allowed_folders_limit_message
                else R.string.allowed_folders_limit_permission_message
            )
            .setPositiveButton(R.string.allowed_folders_limit_continue) { _, _ -> openPicker() }
            .setNegativeButton(R.string.cancel, null)
            .show()
    }

    /**
     * The old storage permission:
     *    - on a device that grants it, it serves the whole card
     *    - it has to cope with a permission that was denied, Android then
     *      shows no dialog at all and answers always denied, so the app
     *      info screen is the only place left to repair it.
     */
    private fun askForFullAccess() {
        if (androidWillNotAskAgain())
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
            // The storage mode may have just changed, so the running server has to be told.
            FsService.restart()
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

    private fun openAppSettings() {
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
        AllowedFolders.takeGrant(requireContext(), treeUri)
        // The storage mode may have just changed, so the running server has to be told.
        FsService.restart()
        refresh()
    }

    private fun removeFolder(tree: StorageTree) {
        AllowedFolders.releaseGrant(requireContext(), AllowedFolders.uriFor(tree))
        FsService.restart()
        Toast.makeText(
            requireContext(),
            getString(R.string.allowed_folders_removed, tree.name),
            Toast.LENGTH_SHORT,
        ).show()
        refresh()
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
    }
}
