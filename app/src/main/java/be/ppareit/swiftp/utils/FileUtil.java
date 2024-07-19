package be.ppareit.swiftp.utils;

import android.annotation.TargetApi;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.UriPermission;
import android.net.Uri;
import android.os.Build;
import android.os.ParcelFileDescriptor;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.documentfile.provider.DocumentFile;
import android.util.Log;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.List;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.server.SessionThread;


public abstract class FileUtil {

    private static final String LOG = "FileUtil";

    /**
     * Copy a file. The target file may even be on external SD card for Kitkat.
     *
     * @param source The source file
     * @param target The target file
     * @return true if the copying was successful.
     */
    @SuppressWarnings("null")
    private static boolean copyFile(final File source, final File target, Context context) {
        FileInputStream inStream = null;
        OutputStream outStream = null;
        FileChannel inChannel = null;
        FileChannel outChannel = null;
        try {
            inStream = new FileInputStream(source);

            // First try the normal way
            if (isWritable(target)) {
                // standard way
                outStream = new FileOutputStream(target);
                inChannel = inStream.getChannel();
                outChannel = ((FileOutputStream) outStream).getChannel();
                inChannel.transferTo(0, inChannel.size(), outChannel);
            } else {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                    // Storage Access Framework
                    DocumentFile targetDocument = getDocumentFile(target, false, context);
                    outStream =
                            context.getContentResolver().openOutputStream(targetDocument.getUri());
                } else if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
                    // Workaround for Kitkat ext SD card

                    Uri uri = MediaStoreHack.getUriFromFile(target.getAbsolutePath(), context);
                    outStream = context.getContentResolver().openOutputStream(uri);
                } else {
                    return false;
                }

                if (outStream != null) {
                    // Both for SAF and for Kitkat, write to output stream.
                    byte[] buffer = new byte[16384]; // MAGIC_NUMBER
                    int bytesRead;
                    while ((bytesRead = inStream.read(buffer)) != -1) {
                        outStream.write(buffer, 0, bytesRead);
                    }
                }

            }
        } catch (Exception e) {
            Log.e(LOG,
                    "Error when copying file from " + source.getAbsolutePath() + " to " + target.getAbsolutePath(), e);
            return false;
        } finally {
            try {
                inStream.close();
            } catch (Exception e) {
                // ignore exception
            }

            try {
                outStream.close();
            } catch (Exception e) {
                // ignore exception
            }

            try {
                inChannel.close();
            } catch (Exception e) {
                // ignore exception
            }

            try {
                outChannel.close();
            } catch (Exception e) {
                // ignore exception
            }
        }
        return true;
    }

    public static FileOutputStream getOutputStream(final File target, Context context) throws FileNotFoundException {
        FileOutputStream outStream = null;
        // First try the normal way
        if (isWritable(target)) {
            // standard way
            outStream = new FileOutputStream(target);
        } else {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                // Storage Access Framework
                DocumentFile targetDocument = getDocumentFile(target, false, context);
                outStream = new ParcelFileDescriptor.AutoCloseOutputStream(context.getContentResolver()
                        .openFileDescriptor(targetDocument.getUri(), "rw"));
            } else if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
                // Workaround for Kitkat ext SD card
                return MediaStoreHack.getOutputStream(context, target.getPath());
            }
        }
        return outStream;
    }

    /**
     * Delete a file. May be even on external SD card.
     *
     * @param file the file to be deleted.
     * @return True if successfully deleted.
     */
    public static boolean deleteFile(@NonNull final File file, Context context) {
        // First try the normal deletion.
        if (file == null) return true;

        if (Util.useScopedStorage()) {
            // Fix: Store overwrite failure happening with internal Android 13 via the old code.
            // Failure happening with new multi user changes happening on sd card not compat with old.
            // Fix: Must move above File to make sure its deleted without incident.
            final DocumentFile df = getDocumentFile(file.getPath());
            if (df != null) return df.delete();
        }

        boolean fileDelete = false;
        if (file.isDirectory()) {
            fileDelete = rmdir(file, context);
        }
        if (file.delete() || fileDelete)
            return true;

        // Try with Storage Access Framework.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && FileUtil.isOnExtSdCard(file, context)) {
            DocumentFile document = getDocumentFile(file, false, context);
            if (document == null) {
                return false;
            }
            return document.delete();
        }

        // Try the Kitkat workaround.
        if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
            ContentResolver resolver = context.getContentResolver();

            try {
                Uri uri = MediaStoreHack.getUriFromFile(file.getAbsolutePath(), context);
                resolver.delete(uri, null, null);
                return !file.exists();
            } catch (Exception e) {
                Log.e(LOG, "Error when deleting file " + file.getAbsolutePath(), e);
                return false;
            }
        }

        return !file.exists();
    }

    private static boolean rename(File source, File target) {
        source.renameTo(target);
        return false;
    }

    /**
     * Move a file. The target file may even be on external SD card.
     *
     * @param source The source file
     * @param target The target file
     * @return true if the copying was successful.
     */
    public static boolean moveFile(@NonNull final File source, @NonNull final File target, Context context) {

        if (Util.useScopedStorage()) {
            // Fix: Block is fix for rename file causing app to crash with latest DocumentFile changes.
            // Fix: Must move above File to make sure it happens without incident, as on Android 8,
            // File was successful but always returns false thus causing various issues.
            DocumentFile df = getDocumentFile(source.getPath());
            if (df != null) {
                try {
                    if (DocumentsContract.renameDocument(context.getContentResolver(), df.getUri(), target.getName()) != null) {
                        return true;
                    }
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                }
            }
        }

        if (target.exists()) {
            return false;
        }

        // First try the normal rename.
        if (rename(source, target)) {
            return true;
        }

        // Try the Storage Access Framework if it is just a rename within the same parent folder.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP
                && source.getParent().equals(target.getParent()) && FileUtil.isOnExtSdCard(source, context)) {
            DocumentFile document = getDocumentFile(source, false, context);
            if (document == null) {
                return false;
            }
            try {
                if (DocumentsContract.renameDocument(context.getContentResolver(), document.getUri(), target.getName()) != null) {
                    return true;
                }
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        // if all else failed, try to copy the file and delete the source file
        if (!copyFile(source, target, context) || !deleteFile(source, context)) {
            return false;
        }

        return true;
    }

    /**
     * Rename a folder. In case of extSdCard in Kitkat, the old folder stays in place, but files are moved.
     *
     * @param source The source folder.
     * @param target The target folder.
     * @return true if the renaming was successful.
     */
    public static boolean renameFolder(@NonNull final File source, @NonNull final File target, Context context) {

        if (Util.useScopedStorage()) {
            // Fix: File rename is having random and odd results here with scoped storage. So,
            // need to move above File.
            // Fix: Store overwrite failure happening with internal Android 13 via the old code.
            // Fix: Failure happening with new multi user changes happening on sd card not compat with old.
            DocumentFile df = getDocumentFile(source.getPath());
            if (df != null) {
                try {
                    if (DocumentsContract.renameDocument(context.getContentResolver(), df.getUri(), target.getName()) != null) {
                        return true;
                    }
                } catch (FileNotFoundException e) {
                    e.printStackTrace();
                }
            }
        }

        // First try the normal rename.
        if (rename(source, target)) {
            return true;
        }
        if (target.exists()) {
            return false;
        }

        // Try the Storage Access Framework if it is just a rename within the same parent folder.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP
                && source.getParent().equals(target.getParent()) && FileUtil.isOnExtSdCard(source, context)) {
            DocumentFile document = getDocumentFile(source, true, context);
            if (document == null) {
                return false;
            }
            try {
                if((DocumentsContract.renameDocument(context.getContentResolver(), document.getUri(),
                        target.getName()) != null)) {
                    return true;
                }
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }

        // Try the manual way, moving files individually.
        if (!mkdir(target, context)) {
            return false;
        }

        File[] sourceFiles = source.listFiles();

        if (sourceFiles == null) {
            return true;
        }

        for (File sourceFile : sourceFiles) {
            String fileName = sourceFile.getName();
            File targetFile = new File(target, fileName);
            if (!copyFile(sourceFile, targetFile, context)) {
                // stop on first error
                return false;
            }
        }
        // Only after successfully copying all files, delete files on source folder.
        for (File sourceFile : sourceFiles) {
            if (!deleteFile(sourceFile, context)) {
                // stop on first error
                return false;
            }
        }
        return true;
    }

    /**
     * Create a folder. The folder may even be on external SD card for Kitkat.
     *
     * @param file The folder to be created.
     * @return True if creation was successful.
     * @deprecated use {@link #mkdirs(Context, File)}
     */
    public static boolean mkdir(final File file, Context context) {
        if (file == null)
            return false;

        if (Util.useScopedStorage()) {
            // Fix: The original code below in the next block is having some failures during Android
            // 13 tests. This works around that and has no more failures.
            // Fix: error on File use in logcat by moving above.
            final String filename = file.getName();
            String parent = file.getPath();
            DocumentFile child = getDocumentFile(file.getPath());
            if (child != null && child.exists()) return false;
            parent = parent.substring(0, parent.length() - filename.length());
            DocumentFile documentFile = getDocumentFile(parent);
            if (documentFile != null) {
                if (documentFile.createDirectory(filename) != null) {
                    return documentFile.exists();
                }
            }
        }

        if (file.exists()) {
            // nothing to create.
            return file.isDirectory();
        }

        // Try the normal way
        if (file.mkdirs()) {
            return true;
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && (FileUtil.isOnExtSdCard(file, context))) {
            // Try with Storage Access Framework.
            DocumentFile document = getDocumentFile(file, true, context);
            if (document == null) {
                return false;
            }
            // getDocumentFile implicitly creates the directory.
            return document.exists();
        }

        // Try the Kitkat workaround.
        if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
            try {
                return MediaStoreHack.mkdir(context, file);
            } catch (IOException e) {
                return false;
            }
        }

        return false;
    }

    public static boolean mkdirs(Context context, File file) {
        boolean isSuccessful = true;
        isSuccessful = mkdir(new File(file.getPath()), context);
        return isSuccessful;
    }

    public static boolean mkfile(final File file, Context context) throws IOException {
        if (file == null)
            return false;
        if (file.exists()) {
            // nothing to create.
            return !file.isDirectory();
        }

        // Try the normal way
        try {
            if (file.createNewFile()) {
                return true;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Try with Storage Access Framework.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && FileUtil.isOnExtSdCard(file, context)) {
            DocumentFile document = getDocumentFile(file.getParentFile(), true, context);
            if (document == null) {
                return false;
            }
            // getDocumentFile implicitly creates the directory.
            try {
                // use for SingleDocumentFile
                return DocumentsContract.createDocument(context.getContentResolver(), document.getUri(), DocumentsContract.Document.COLUMN_MIME_TYPE, file.getName()) != null;
                // return document.createFile("image", file.getName()) != null;
            } catch (Exception e) {
                e.printStackTrace();
                return false;
            }
        }

        if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
            try {
                return MediaStoreHack.mkfile(context, file);
            } catch (Exception e) {
                return false;
            }
        }
        return false;
    }

    /*
    * File creation for DocumentFile from File.
    * */
    public static DocumentFile mkfile(final File file, String mime) throws IOException {
        try {
            // May not be able to create a file in dirs that have eg "?". However, can create a
            // file in a dir that has the # symbol.
            final String filename = file.getName();
            final String path = file.getPath();
            DocumentFile parent = getDocumentFile(path.substring(0, path.lastIndexOf(File.separator)));
            if (parent != null) return parent.createFile(mime, filename);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Delete a folder.
     *
     * @param file The folder name.
     * @return true if successful.
     */
    private static boolean rmdir(@NonNull final File file, Context context) {
        if (!file.exists()) return true;

        File[] files = file.listFiles();
        if (files != null && files.length > 0) {
            for (File child : files) {
                rmdir(child, context);
            }
        }

        // Try the normal way
        if (file.delete()) {
            return true;
        }

        // Try with Storage Access Framework.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            DocumentFile document = getDocumentFile(file, true, context);
            if (document != null && document.delete()) {
                return true;
            }
        }

        // Try the Kitkat workaround.
        if (Build.VERSION.SDK_INT == Build.VERSION_CODES.KITKAT) {
            ContentResolver resolver = context.getContentResolver();
            ContentValues values = new ContentValues();
            values.put(MediaStore.MediaColumns.DATA, file.getAbsolutePath());
            resolver.insert(MediaStore.Images.Media.EXTERNAL_CONTENT_URI, values);

            // Delete the created entry, such that content provider will delete the file.
            resolver.delete(MediaStore.Files.getContentUri("external"), MediaStore.MediaColumns.DATA + "=?",
                    new String[]{file.getAbsolutePath()});
        }

        return !file.exists();
    }

    /**
     * Check if a file is readable.
     *
     * @param file The file
     * @return true if the file is readable.
     */
    public static boolean isReadable(final File file) {
        if (file == null)
            return false;
        if (!file.exists()) return false;

        boolean result;
        try {
            result = file.canRead();
        } catch (SecurityException e) {
            return false;
        }

        return result;
    }

    /**
     * Check if a file is writable. Detects write issues on external SD card.
     *
     * @param file The file
     * @return true if the file is writable.
     */
    public static boolean isWritable(final File file) {
        if (file == null)
            return false;
        boolean isExisting = file.exists();

        try {
            FileOutputStream output = new FileOutputStream(file, true);
            try {
                output.close();
            } catch (IOException e) {
                // do nothing.
            }
        } catch (FileNotFoundException e) {
            return false;
        }
        boolean result = file.canWrite();

        // Ensure that file is not created during this process.
        if (!isExisting) {
            file.delete();
        }

        return result;
    }

    // Utility methods for Android 5

    /**
     * Check for a directory if it is possible to create files within this directory, either via normal writing or via
     * Storage Access Framework.
     *
     * @param folder The directory
     * @return true if it is possible to write in this directory.
     */
    public static boolean isWritableNormalOrSaf(final File folder, Context c) {
        // Verify that this is a directory.
        if (folder == null)
            return false;
        if (!folder.exists() || !folder.isDirectory()) {
            return false;
        }

        // Find a non-existing file in this directory.
        int i = 0;
        File file;
        do {
            String fileName = "AugendiagnoseDummyFile" + (++i);
            file = new File(folder, fileName);
        } while (file.exists());

        // First check regular writability
        if (isWritable(file)) {
            return true;
        }

        // Next check SAF writability.
        DocumentFile document = getDocumentFile(file, false, c);

        if (document == null) {
            return false;
        }

        // This should have created the file - otherwise something is wrong with access URL.
        boolean result = document.canWrite() && file.exists();

        // Ensure that the dummy file is not remaining.
        deleteFile(file, c);
        return result;
    }

    /**
     * Get a list of external SD card paths. (Kitkat or higher.)
     *
     * @return A list of external SD card paths.
     */
    @TargetApi(Build.VERSION_CODES.KITKAT)
    private static String[] getExtSdCardPaths(Context context) {
        List<String> paths = new ArrayList<>();
        for (File file : context.getExternalFilesDirs("external")) {
            if (file != null && !file.equals(context.getExternalFilesDir("external"))) {
                int index = file.getAbsolutePath().lastIndexOf("/Android/data");
                if (index < 0) {
                    Log.w(LOG, "Unexpected external file dir: " + file.getAbsolutePath());
                } else {
                    String path = file.getAbsolutePath().substring(0, index);
                    try {
                        path = new File(path).getCanonicalPath();
                    } catch (IOException e) {
                        // Keep non-canonical path.
                    }
                    paths.add(path);
                }
            }
        }
        if (paths.isEmpty()) paths.add("/storage/sdcard1");
        return paths.toArray(new String[0]);
    }

    @TargetApi(Build.VERSION_CODES.KITKAT)
    public static String[] getExtSdCardPathsForActivity(Context context) {
        List<String> paths = new ArrayList<>();
        for (File file : context.getExternalFilesDirs("external")) {
            if (file != null) {
                int index = file.getAbsolutePath().lastIndexOf("/Android/data");
                if (index < 0) {
                    Log.w(LOG, "Unexpected external file dir: " + file.getAbsolutePath());
                } else {
                    String path = file.getAbsolutePath().substring(0, index);
                    try {
                        path = new File(path).getCanonicalPath();
                    } catch (IOException e) {
                        // Keep non-canonical path.
                    }
                    paths.add(path);
                }
            }
        }
        if (paths.isEmpty()) paths.add("/storage/sdcard1");
        return paths.toArray(new String[0]);
    }

    /**
     * Determine the main folder of the external SD card containing the given file.
     *
     * @param file the file.
     * @return The main folder of the external SD card containing this file, if the file is on an SD card. Otherwise,
     * null is returned.
     */
    @TargetApi(Build.VERSION_CODES.KITKAT)
    private static String getExtSdCardFolder(final FileUtil.Gen file, Context context) {
        boolean isDocFile = file.getOb() instanceof DocumentFile; // double check
        if (Util.useScopedStorage() && isDocFile) {
            return getExtSdCardFolderScopedStorage(file);
        } else {
            return getExtSdCardFolderOld(file, context);
        }
    }

    public static String getExtSdCardFolderScopedStorage(final FileUtil.Gen file) {
        try {
            String s = file.getCanonicalPath();
            String base = getSdCardBaseFolderScopedStorage();
            String name = getSdCardNameScopedStorage();
            if (name != null && !name.isEmpty() && s.contains(name)) {
                String base2 = getTreeUriStoragePath((DocumentFile) file.getOb());
                if (base2 != null) {
                    if (base2.contains(File.separator)) {
                        base2 = base2.substring(0, base2.lastIndexOf(File.separator));
                    }
                    base2 = base2.substring(0, base2.indexOf(name) + name.length());
                    // need to return eg "/storage/sd card"
                    return base + File.separator + base2;
                }
            }
        } catch (IOException e) {
            //
        }
        return null;
    }

    /*
     * Used to get eg "/storage" since URIs for whatever reasons omit this.
     * */
    public static String getSdCardBaseFolderScopedStorage() {
        final File[] f = ContextCompat.getExternalFilesDirs(App.getAppContext(), null);
        String path = null;
        for (File each : f) {
            final String eachPath = each.getPath();
            final Uri uri = FileUtil.getTreeUri();
            if (uri == null) return null;
            String s = cleanupUriStoragePath(uri);
            try {
                // Only want the sd card folder
                if (s != null) s = s.substring(0, s.indexOf(File.pathSeparator));
            } catch (Exception e) {
                return null;
            }
            if (s != null) {
                if (eachPath.contains(s)) {
                    // Found the sd card path eg "/storage/[sd card]/Temp" should return as "/storage"
                    // as the DocumentFile Uri will contain the [sd card] all the way to the picked folder.
                    path = eachPath.substring(0, eachPath.indexOf(s) - 1);
                }
            }
        }
        return path;
    }

    /*
     * Obtains only the sd card folder name or null if sd card is not being used
     * */
    public static String getSdCardNameScopedStorage() {
        final Uri uri = FileUtil.getTreeUri();
        if (uri == null) return null;
        String s = cleanupUriStoragePath(uri);
        if (s.contains("primary:")) return null; // It is definitely not on the sd card.
        try {
            // Only want the sd card folder name
            s = s.substring(0, s.indexOf(File.pathSeparator));
            return s;
        } catch (Exception e) {
            return null;
        }
    }

    private static String getExtSdCardFolderOld(final FileUtil.Gen file, Context context) {
        String[] extSdPaths = getExtSdCardPaths(context);
        try {
            for (int i = 0; i < extSdPaths.length; i++) {
                if (file.getCanonicalPath().startsWith(extSdPaths[i])) {
                    return extSdPaths[i]; // returns eg "/storage/42F3-5677"
                }
            }
        } catch (IOException e) {
            return null;
        }
        return null;
    }

    /**
     * Determine if a file is on external sd card. (Kitkat or higher.)
     *
     * @param file The file.
     * @return true if on external sd card.
     */
    @TargetApi(Build.VERSION_CODES.KITKAT)
    public static boolean isOnExtSdCard(final File file, Context c) {
        return getExtSdCardFolder(new Gen<>(file), c) != null;
    }

    /**
     * Get a DocumentFile corresponding to the given file (for writing on ExtSdCard on Android 5). If the file is not
     * existing, it is created.
     *
     * @param file        The file.
     * @param isDirectory flag indicating if the file should be a directory.
     * @return The DocumentFile
     */
    @TargetApi(Build.VERSION_CODES.LOLLIPOP)
    public static DocumentFile getDocumentFile(final File file, final boolean isDirectory, Context context) {
        String baseFolder = getExtSdCardFolder(new Gen<>(file), context);
        boolean originalDirectory = false;
        if (baseFolder == null) {
            return null;
        }

        String relativePath = null;
        try {
            String fullPath = file.getCanonicalPath();
            if (!baseFolder.equals(fullPath))
                relativePath = fullPath.substring(baseFolder.length() + 1);
            else originalDirectory = true;
        } catch (IOException e) {
            return null;
        } catch (Exception f) {
            originalDirectory = true;
            //continue
        }
        String as = FsSettings.getExternalStorageUri();

        Uri treeUri = null;
        if (as != null) treeUri = Uri.parse(as);
        if (treeUri == null) {
            return null;
        }
        if (file.exists()) {
            Uri documentUri = DocumentsContract.buildDocumentUriUsingTree(treeUri, DocumentsContract.getTreeDocumentId(treeUri) + relativePath);
            DocumentFile document = DocumentFile.fromSingleUri(context, documentUri);
            if (document != null) {
                return document;
            }
        }

        // start with root of SD card and then parse through document tree.
        DocumentFile document = DocumentFile.fromTreeUri(context, treeUri);
        if (originalDirectory) return document;
        String[] parts = relativePath.split("\\/");
        for (int i = 0; i < parts.length; i++) {
            DocumentFile nextDocument = document.findFile(parts[i]);

            if (nextDocument == null) {
                if ((i < parts.length - 1) || isDirectory) {
                    nextDocument = document.createDirectory(parts[i]);
                } else {
                    nextDocument = document.createFile(DocumentsContract.Document.COLUMN_MIME_TYPE, parts[i]);
                }
            }
            document = nextDocument;
        }

        return document;
    }

    /*
     * Basic switch out of File to DocumentFile. Can't always be used as the File Uri isn't compatible
     * with scoped storage. But, won't cause security failure for specific uses.
     * */
    public static DocumentFile getDocumentFileFromFileScopedStorage(File f) {
        if (Util.useScopedStorage()) {
            return FileUtil.getDocumentFile(f.getPath());
        }
        return null;
    }

    public static DocumentFile getDocumentFileFromUri(Uri uri) {
        // For DocumentFile, to move into sub folders, the user path cannot start with /
        // Say a user enters in "/test" inside the client, the string that is received will be "test".
        // findFile("test") for getting the sub dir "test" contents works.
        if (uri != null) return DocumentFile.fromTreeUri(App.getAppContext(), uri);
        return null;
    }

    // Utility methods for Kitkat

    /**
     * Checks whether the target path exists or is writable
     *
     * @param f       the target path
     * @param context
     * @return 1 if exists or writable, 0 if not writable
     */
    public static int checkFolder(final String f, Context context) {
        if (f == null) return 0;

        File folder = new File(f);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && FileUtil.isOnExtSdCard(folder, context)) {
            if (!folder.exists() || !folder.isDirectory()) {
                return 0;
            }

            // On Android 5, trigger storage access framework.
            if (FileUtil.isWritableNormalOrSaf(folder, context)) {
                return 1;

            }
        } else if (Build.VERSION.SDK_INT == 19 && FileUtil.isOnExtSdCard(folder, context)) {
            // Assume that Kitkat workaround works
            return 1;
        } else if (folder.canWrite()) {
            return 1;
        } else {
            return 0;
        }
        return 0;
    }

    /*
     * Retrieves the URI assigned to a user upon connection that is required to use scoped storage
     * and get the correct URI to work with.
     * */
    public static Uri getTreeUri() {
        String threadName = Thread.currentThread().getName();
        String userUriString = SessionThread.getUriString(threadName);
        if (userUriString == null || userUriString.isEmpty()) return null;
        List<UriPermission> list = App.getAppContext().getContentResolver().getPersistedUriPermissions();
        if (list.size() > 0 && list.get(0) != null) {
            for (UriPermission perm : list) {
                String uriString = perm.getUri().getPath();
                if (uriString == null) continue;
                if (uriString.equals(userUriString)) {
                    return perm.getUri();
                }
            }
        }
        return null;
    }

    /*
     * HUGE performance improvement over DocumentFile.findFile().
     * Use at least until Google fixes DocumentFile.findFile() performance (should it ever happen).
     */
    public static DocumentFile getDocumentFile(String filename) {
        final Uri uri = getTreeUri();
        if (uri == null) return null;
        final String uriString = uri.getPath();
        if (uriString == null) return null;
        final String s = filename.contains(File.pathSeparator)
                ? filename
                : convertFilePathToUriString(uriString, filename);
        if (s == null) return null;
        final Uri finalUri = DocumentsContract.buildDocumentUriUsingTree(uri, s);
        return getDocumentFileFromUri(finalUri);
    }

    /*
    * Helper method to switch File path type to uriString.
    * */
    private static String convertFilePathToUriString(String mTree, String oldPath) {

        String tree = mTree.contains("/tree/") ? mTree.replaceFirst("/tree/", "") : mTree;
        if (tree.contains("primary:")) tree = tree.replaceFirst("primary:", "/storage/emulated/0/");
        else tree = tree.replaceFirst(File.pathSeparator, File.separator);
        if (oldPath.contains(tree)) {
            final String s = oldPath.substring(oldPath.indexOf(tree) + tree.length());
            if (tree.contains("/storage/emulated/0/")) tree = tree.replaceFirst("/storage/emulated/0/", "primary:");
            else tree = tree.replaceFirst(File.separator, File.pathSeparator);
            return tree + s;
        }
        return null;
    }

    /*
     * Cleans up a Uri tree path that contains non-path stuff such as "tree:",  ":", etc.
     */
    public static String cleanupUriStoragePath(Uri uri) {
        if (uri == null) return "";
        String s = uri.getPath();
        if (s != null && s.contains("tree:")) s = s.replace("tree:", "");
        else if (s != null && s.contains("/tree/")) s = s.replace("/tree/", "");
        if (s != null && s.contains(File.pathSeparator)) {
            String s2 = s.substring(0, s.indexOf(File.pathSeparator) - 1);
            s = s.substring(s.lastIndexOf(s2));
        }
        return s;
    }

    /*
     * Gets the picker path directly from the DocumentFile which doesn't include eg "/storage/"
     */
    public static String getTreeUriStoragePath(DocumentFile file) {
        if (file == null) return null;
        String path = cleanupUriStoragePath(file.getUri()); // gets path from user root on out
        if (path != null) {
            // Make compatible with sub folders
            // "primary:" seems to be internal verses just the ":" found with sd card.
            // Well, sd card use could also be explained as "[sd card name]:" verses "[name for internal]:"
            if (path.contains("primary:")) {
                // Internal storage
                path = path.replace("primary:", "");
            } else if (path.contains(":")) {
                // We can know sd card path from the Uri so nothing else is needed :)
                path = path.replace(":", File.separator);
            }
            // Need eg /storage/emulated/0/ for internal or /storage/ for sd card
            return path;
        }
        return null;
    }

    /*
     * Gets the full File type path using the DocumentFile including eg "/storage/ and all the way
     * to the picker folder.
     */
    public static String getFileTypePathFromDocumentFile(DocumentFile file) {
        String cleaned = cleanupUriStoragePath(file.getUri());
        if (cleaned.contains("primary:")) {
            return cleaned.replaceFirst("primary:", "/storage/emulated/0/");
        }
        return "/storage/" + cleaned.replaceFirst(File.pathSeparator, File.separator);
    }

    /**
     * Used to deduplicate some File and DocumentFile methods as eg .isDirectory() applies to both
     * Thus, it gets rid of some horrible duplication that would have happened otherwise.
     */
    public static class Gen<T> {
        T ob;

        public Gen(T o1) {
            ob = o1;
        }

        public T getOb() {
            return ob;
        }

        public boolean exists() {
            Object ob = getOb();
            if (ob == null) return false;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).exists();
            else return ((File) ob).exists();
        }

        public String getName() {
            Object ob = getOb();
            if (ob == null) return "";
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).getName();
            else return ((File) ob).getName();
        }

        public boolean isDirectory() {
            Object ob = getOb();
            if (ob == null) return false;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).isDirectory();
            else return ((File) ob).isDirectory();
        }

        public long length() {
            Object ob = getOb();
            if (ob == null) return 0;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).length();
            else return ((File) ob).length();
        }

        public long lastModified() {
            Object ob = getOb();
            if (ob == null) return 0;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).lastModified();
            else return ((File) ob).lastModified();
        }

        public boolean isFile() {
            Object ob = getOb();
            if (ob == null) return false;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).isFile();
            else return ((File) ob).isFile();
        }

        public boolean canRead() {
            Object ob = getOb();
            if (ob == null) return false;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).canRead();
            else return ((File) ob).canRead();
        }

        public boolean canWrite() {
            Object ob = getOb();
            if (ob == null) return false;
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).canWrite();
            else return ((File) ob).canWrite();
        }

        public String getCanonicalPath() throws IOException {
            Object ob = getOb();
            if (ob == null) return "";
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).getUri().getPath();
            else return ((File) ob).getCanonicalPath();
        }

        public String getAbsolutePath() {
            Object ob = getOb();
            if (ob == null) return "";
            if (ob instanceof DocumentFile) return ((DocumentFile) getOb()).getUri().getPath();
            else return ((File) ob).getAbsolutePath();
        }
    }

    public static Gen convertDocumentFileToGen(DocumentFile f) {
        return new Gen<>(f);
    }

    public static Gen convertFileToGen(File f) {
        return new Gen<>(f);
    }

    /*
     * Use under certain conditions only as File use isn't fully compatible with scoped storage and
     * will throw security exceptions under incorrect use.
     * */
    public static Gen createGenFromFile(File f) {
        if (Util.useScopedStorage()) {
            return FileUtil.convertDocumentFileToGen(FileUtil.getDocumentFileFromFileScopedStorage(f));
        } else {
            return FileUtil.convertFileToGen(f);
        }
    }
}