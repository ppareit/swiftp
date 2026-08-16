package be.ppareit.swiftp.gui

import android.app.Activity
import android.app.AlertDialog
import android.content.Context
import android.content.Intent
import android.os.Bundle
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
import be.ppareit.swiftp.utils.StorageProbe
import be.ppareit.swiftp.utils.StorageTree

/**
 * Android keeps the persisted URI permissions and this screen shows them.
 */
class AllowedFoldersFragment : Fragment() {

    private lateinit var listView: ListView
    private lateinit var explanation: TextView

    override fun onCreateView(
        inflater: LayoutInflater,
        container: ViewGroup?,
        savedInstanceState: Bundle?,
    ): View {
        val root = inflater.inflate(R.layout.allowed_folders_list_layout, container, false)
        listView = root.findViewById(R.id.allowed_folders_list)
        explanation = root.findViewById(R.id.allowed_folders_explanation)

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
        explanation.setText(
            when {
                folders.isEmpty() && StorageProbe.hasFullSdCardAccess() ->
                    R.string.allowed_folders_full_sdcard_explanation
                folders.isEmpty() -> R.string.allowed_folders_none_explanation
                else -> R.string.allowed_folders_some_explanation
            }
        )
        // A copy: ArrayAdapter keeps the list it is given and will mutate it, and this one
        // belongs to the cached AllowedFolders snapshot that the server reads from.
        listView.adapter =
            FolderAdapter(requireContext(), folders.toMutableList(), ::removeFolder)
    }

    /**
     * On a device that can already serve everything, the *first* folder is the one that switches
     * the app to SAF, and from then on only the chosen folders are served. So the 'Add Folder'
     * fab is limiting the allowed folders.
     */
    private fun pickFolder() {
        if (AllowedFolders.isEmpty() && StorageProbe.hasFullSdCardAccess()) {
            AlertDialog.Builder(requireContext())
                .setTitle(R.string.allowed_folders_limit_title)
                .setMessage(R.string.allowed_folders_limit_message)
                .setPositiveButton(R.string.allowed_folders_limit_continue) { _, _ -> openPicker() }
                .setNegativeButton(R.string.cancel, null)
                .show()
            return
        }
        openPicker()
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
    }
}
