package be.ppareit.swiftp.utils;

import android.annotation.TargetApi;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.UriPermission;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.ParcelFileDescriptor;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;
import androidx.documentfile.provider.DocumentFile;
import android.util.Log;
import android.util.Pair;


import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.Util;


public abstract class FileUtil {

    private static final String LOG = "FileUtil";

    public enum TYPE {
        file,
        dir
    }

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
        if (file.exists()) {
            // nothing to create.
            return file.isDirectory();
        }

        // Try the normal way
        if (file.mkdirs()) {
            return true;
        }

        // Try with Storage Access Framework.
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && FileUtil.isOnExtSdCard(file, context)) {
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

    // Performance improvement with DocumentFile. As we can avoid a finding of the file this way since
    // create will far more quickly create it and give it along with that. Thus, needed a way to return
    // DocumentFile at creation time.
    public static DocumentFile mkfile(final File file, Context context, String mime) throws IOException {
        try {
            final String filename = file.getName();
            // Travel only to the dir for where the file goes:
            Uri uri = getFullCWDUri("", file.getPath().substring(0, file.getPath().lastIndexOf(File.separator)));
            // Can't create a file in dirs that have eg the symbol "?".
            //  Can create a folder with it.
            //  Can even create a file in a dirs that have # symbol.
            DocumentFile dfile = FileUtil.getDocumentFileFromUri(uri).createFile(mime, filename);
            if (dfile != null) return dfile;
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
        String relativePath;
        if (baseFolder == null) {
            return null;
        }

        Pair<String, Boolean> pair = getRelativePath(context, file, baseFolder);
        if (pair == null) return null;
        relativePath = pair.first;
        originalDirectory = pair.second;

        String as = FsSettings.getExternalStorageUri();

        Uri treeUri = null;
        if (as != null) treeUri = Uri.parse(as);
        if (Util.useScopedStorage() && treeUri == null) {
            // Fix Android 8.0 sd card having null with FsSettings.getExternalStorageUri()
            // The rest works fine after this
            treeUri = getTreeUri();
        }
        if (treeUri == null) return null;

        if (file.exists()) {
            Uri documentUri = DocumentsContract.buildDocumentUriUsingTree(treeUri, DocumentsContract.getTreeDocumentId(treeUri) + relativePath);
            DocumentFile document = DocumentFile.fromSingleUri(context, documentUri);
            if (document != null) {
                return document;
            }
        }

        // start with root of SD card and then parse through document tree.
        DocumentFile document = FileUtil.getDocumentFileFromUri(treeUri);
        if (originalDirectory) return document;
        String[] parts = relativePath.split("\\/");
        for (int i = 0; i < parts.length; i++) {
            DocumentFile nextDocument = document.findFile(parts[i]);

            if (nextDocument == null) {
                if ((i < parts.length - 1) || isDirectory) {
                    if (!parts[i].isEmpty()) {
                        nextDocument = document.createDirectory(parts[i]);
                    } else {
                        continue; // quick fix for an empty part causing major problems.
                    }
                } else {
                    nextDocument = document.createFile(DocumentsContract.Document.COLUMN_MIME_TYPE, parts[i]);
                }
            }
            document = nextDocument;
        }

        return document;
    }

    private static Pair<String, Boolean> getRelativePath(Context context, File file, String baseFolder) {
        String relativePath = "";
        boolean originalDirectory = false;
        if (Util.useScopedStorage()) {
            // Currently, relativePath needs to be "/file" for file or "" if no file is in
            // fullPath for the following code to work right.
            try {
                Uri uri = getTreeUri();
                if (uri != null) {
                    DocumentFile dir = DocumentFile.fromSingleUri(context, uri);
                    if (dir != null) {
                        final String fullPath = file.getCanonicalPath();
                        String clientPath = getScopedClientPath(file.getPath(), null, null);
                        final String DocumentFilePath = getUriStoragePathFullFromDocumentFile(dir, clientPath);
                        if (DocumentFilePath != null) {
                            relativePath = fullPath.replace(DocumentFilePath, "");
                        }
                    }
                }
            } catch (IOException e) {
                return null;
            } catch (Exception f) {
                originalDirectory = true;
                //continue
            }
        } else {
            // This following code isn't working right on Android 8.0 sd card use. Bypassed with
            // override of Android 11+ new storage use which has tested successfully as backward
            // compatible with 8.0 sd card.
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
        }
        return new Pair<>(relativePath, originalDirectory);
    }

    /*
     * The param is tangled with the paths at times so that is necessary to include at specific times.
     * The meaning can change depending on slash or no slash since the param contains files or just dirs.
     * */
    public static DocumentFile getDocumentFileWithParamScopedStorage(String param, @Nullable String cwd, String clientPath) {
        DocumentFile f = null;
        // affected by param having slash or not; empty or not
        Uri uri = FileUtil.getFullCWDUri(cwd != null ? cwd : param, clientPath);
        if (uri != null) {
            try {
                // Get the file
                String mParam = param;
                if (param.contains(File.separator)) {
                    try {
                        mParam = param.substring(param.lastIndexOf(File.separator) + 1);
                    } catch (NullPointerException e) {
                        mParam = null;
                    }
                }
                // param affects above but down there its file only:
                f = FileUtil.customFindFile(uri, (mParam != null ? mParam : param), FileUtil.TYPE.file, clientPath);
                if (f == null)
                    f = FileUtil.getDocumentFileFromUri(uri).findFile((mParam != null ? mParam : param));
            } catch (NullPointerException e) {
                //
            }
        }
        return f;
    }

    /*
     * Basic switch out of File to DocumentFile. Can't always be used as the File Uri isn't compatible
     * with scoped storage. But, won't cause security failure for specific uses.
     * */
    public static DocumentFile getDocumentFileFromFileScopedStorage(File f, String clientPath) {
        if (Util.useScopedStorage()) {
            Uri uri = FileUtil.getFullCWDUri(f.getPath(), clientPath);
            if (uri != null) return FileUtil.getDocumentFileFromUri(uri);
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
     * Retrieves the Uri required to use scoped storage.
     * */
    public static Uri getTreeUri() {
        // Does not change unless user uses the picker again
        List<UriPermission> list = App.getAppContext().getContentResolver().getPersistedUriPermissions();
        if (list.size() > 0 && list.get(0) != null) {
            // Get the picker path and then tack on any user provided path in the client.
            return list.get(0).getUri();
        }
        return null;
    }

    /*
     * Uses DocumentFile.findFile() to get the Uri for the target dir but does it the slow way as
     * findFile() has a major performance issue.
     */
    public static Uri getFullCWDUriSlow(String param) {
        Uri uri = getTreeUri();
        if (uri != null) {
            try {
                String s = getScopedClientPath(param, null, null);
                if (s.isEmpty() && param.contains(File.separator)) {
                    s = param.substring(0, param.lastIndexOf(File.separator));
                }
                String[] split = s.split(File.separator);
                for (String value : split) {
                    if (value.isEmpty()) continue;
                    String part = value.replaceAll(File.separator, "");
                    DocumentFile dFile = FileUtil.getDocumentFileFromUri(uri).findFile(part);
                    if (dFile != null) uri = dFile.getUri();
                }
                return uri;
            } catch (NullPointerException e) {
                //
            }
        }
        return null;
    }

    /*
     * Mainly uses DocumentsContract to move through the dir's and files for speed.
     * On an issue - should the faster but complex code fail - falls back to the incredibly slower
     * DocumentFile.findFile() to get it.
     */
    public static Uri getFullCWDUri(String param, String clientPath) {
        Uri uri = getTreeUri();
        if (uri != null) {
            try {
                final int paramDirCount = param.split(File.separator).length;
                final boolean makeParamNull = param.isEmpty() || paramDirCount > 1;
                final String mParam = makeParamNull ? null : param;
                final boolean isDir = param.contains(File.separator) || param.isEmpty();
                final TYPE type = isDir ? TYPE.dir : TYPE.file;
                final DocumentFile result = customFindFile(uri, mParam, type, clientPath);
                if (result != null) {
                    Uri cwdUri = result.getUri();
                    // Double check and do the slow way if needed
                    String s = getScopedClientPath(clientPath, null, null);
                    if (s.isEmpty() && param.contains(File.separator)) {
                        s = param.substring(0, param.lastIndexOf(File.separator));
                    }
                    String[] split = s.split(File.separator);
                    String lastSubDir = split[split.length - 1];
                    final String path = cwdUri.getPath();
                    if (path != null && !path.contains(lastSubDir)) return getFullCWDUriSlow(param);
                    return result.getUri();
                }
            } catch (NullPointerException e) {
                //
            }
        }
        return null;
    }

    /*
     * https://stackoverflow.com/questions/41096332/issues-traversing-through-directory-hierarchy-with-android-storage-access-framew
     * Thanks to spring.ace for providing an example.
     * HUGE performance improvement over DocumentFile.findFile().
     * Use at least until Google fixes DocumentFile.findFile() performance (should it ever happen).
     */
    public static DocumentFile customFindFile(Uri dirUri, @Nullable String filename, TYPE type, String clientPath) {
        ContentResolver contentResolver = App.getAppContext().getContentResolver();
        Uri uri = DocumentsContract.buildChildDocumentsUriUsingTree(dirUri,DocumentsContract
                .getTreeDocumentId(dirUri));
        DocumentFile f = null;
        List<Uri> uriList = new LinkedList<>();
        uriList.add(uri);
        final String chroot = FsSettings.getDefaultChrootDir().getPath();
        String userPath = getScopedClientPath(clientPath, null, null);
        if (userPath.contains(chroot)) userPath = userPath.replace(chroot, "");
        if (userPath.contains(File.separator)) {
            // Stop initial empty from being in the array as that messes things up
            if (userPath.startsWith(File.separator))
                userPath = userPath.replaceFirst(File.separator, "");
        }
        String[] userPathEachDir = userPath.split(File.separator);
        if (userPathEachDir.length == 1 && userPathEachDir[0].isEmpty()) {
            userPathEachDir = new String[0];
        }
        final int userPathMax = userPathEachDir.length;
        int userPathLoc = 0;
        boolean dirMovementFinished = userPathMax == 0 && type == TYPE.file;
        boolean end = false;
        while (!end) {
            uri = uriList.remove(0);
            try (Cursor c = contentResolver.query(uri, new String[]{
                            DocumentsContract.Document.COLUMN_DOCUMENT_ID,
                            DocumentsContract.Document.COLUMN_DISPLAY_NAME,
                            DocumentsContract.Document.COLUMN_MIME_TYPE},
                    null, null, null)) {
                while (c!= null && c.moveToNext()) {
                    final String docId = c.getString(0);
                    final String name = c.getString(1);
                    final String mime = c.getString(2);
                    if (isDirectory(mime) && userPathLoc < userPathMax) {
                        // Move to the target folder from the chosen dir using user supplied path
                        if (isMatchFound(name, userPathEachDir[userPathLoc])) {
                            // Add so it can move into on next loop
                            userPathLoc++;
                            final Uri mUri = DocumentsContract.buildChildDocumentsUriUsingTree(dirUri, docId);
                            uriList.add(mUri);
                            if (userPathLoc == userPathMax) {
                                // If type is dir, then once path is found - including any sub
                                // path - it should stop here. Whereas file should continue.
                                dirMovementFinished = true;
                                if (type == TYPE.dir) {
                                    // Now we can find the dir and return that
                                    Uri mUri2 = DocumentsContract.buildDocumentUriUsingTree(uri, docId);
                                    f = FileUtil.getDocumentFileFromUri(mUri2);
                                    end = true;
                                    break;
                                }
                            }
                        }
                    } else if (dirMovementFinished && isMatchFound(name, filename)) {
                        // Now we can find the file and return the Uri for that
                        Uri mUri = DocumentsContract.buildDocumentUriUsingTree(uri, docId);
                        f = FileUtil.getDocumentFileFromUri(mUri);
                        end = true;
                        break;
                    } else if (type == TYPE.dir && isMatchFound(name, filename != null ? filename : userPath)) {
                        // Now we can find the file and return the Uri for that
                        Uri mUri = DocumentsContract.buildDocumentUriUsingTree(uri, docId);
                        f = FileUtil.getDocumentFileFromUri(mUri);
                        end = true;
                        break;
                    }
                }
            }
            if (uriList.isEmpty()) end = true;
        }
        if (type == TYPE.file && f != null && f.isDirectory()) return null;
        // Fix for empty folder with no user provided client path:
        if (type == TYPE.dir && f == null) {
            f = FileUtil.getDocumentFileFromUri(dirUri);
        }
        return f;
    }

    /*
     * Checks if DocumentsContract entry is a dir or not
     */
    private static boolean isDirectory(String mimeType) {
        // As DocumentsContract.buildChildDocumentsUriUsingTree() states:
        // "Must be a directory with MIME type of DocumentsContract.Document.MIME_TYPE_DIR."
        return DocumentsContract.Document.MIME_TYPE_DIR.equals(mimeType);
    }

    /*
     * Compares two Strings to see if they match
     */
    public static boolean isMatchFound(String s1, String s2) {
        return s1.equals(s2);
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
     * Gets the full path using the DocumentFile including eg "/storage/ and all the way to the picker folder
     */
    public static String getUriStoragePathFullFromDocumentFile(DocumentFile file, String param) {
        String partial = getTreeUriStoragePath(file);
        if (partial != null) {
            String chrootPath = FsSettings.getDefaultChrootDir().getPath();
            // Fix an issue with older Android versions using the new code getting the path wrong
            // here because of using Manage users chroot field. Chroot will contain the whole path.
            String preLastDir = partial;
            String lastDir = partial;
            if (partial.contains(File.separator)) {
                preLastDir = partial.substring(0, partial.lastIndexOf(File.separator));
                lastDir = partial.substring(partial.lastIndexOf(File.separator));
            }
            if (chrootPath.contains(preLastDir) && !chrootPath.endsWith(lastDir)) {
                // Moving up or file
                return chrootPath + lastDir;
            } else if (chrootPath.endsWith(partial) && partial.split(File.separator).length > 1) {
                // Don't duplicate
                return chrootPath;
            }
            // Need eg /storage/emulated/0/TestMain/TestSub/ or /storage/3abc-sdcard/Test/
            final boolean isSDCard = getSdCardNameScopedStorage() != null;
            if (isSDCard) return chrootPath + File.separator + partial;
            else {
                // Includes various checks & fixes for variances as seen on one device and internal
                if (!chrootPath.endsWith(File.separator)) chrootPath += File.separator;
                if (!param.contains(File.separator)) {
                    return chrootPath; // file (dirs only here.) or empty
                }
                final String clientPath = getScopedClientPath(param, null, null);
                if (clientPath.startsWith(File.separator) && chrootPath.endsWith(File.separator)) {
                    chrootPath = chrootPath.substring(0, chrootPath.length() - 1);
                }
                return chrootPath + clientPath;
            }
        }
        return null;
    }

    /**
     * Returns the client path or empty if at chroot from any of the supplied values.
     * At the very least:
     * @param s must be supplied which is the full path of the current dir.
     * */
    public static String getScopedClientPath(String s, @Nullable File f, @Nullable String t) {
        if (s == null || s.isEmpty()) return ""; // at root
        String tree;
        if (t != null) tree = t;
        else tree = FileUtil.cleanupUriStoragePath(FileUtil.getTreeUri());
        if (tree.contains(File.pathSeparator)) tree = tree.substring(tree.indexOf(File.pathSeparator) + 1);
        String param;
        if (f != null) param = f.toString();
        else param = s;
        if (param.contains(tree)) param = param.substring(param.indexOf(tree) + tree.length());
        return param;
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
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).exists();
            else return ((File) getOb()).exists();
        }

        public String getName() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).getName();
            else return ((File) getOb()).getName();
        }

        public boolean isDirectory() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).isDirectory();
            else return ((File) getOb()).isDirectory();
        }

        public long length() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).length();
            else return ((File) getOb()).length();
        }

        public long lastModified() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).lastModified();
            else return ((File) getOb()).lastModified();
        }

        public boolean isFile() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).isFile();
            else return ((File) getOb()).isFile();
        }

        public boolean canRead() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).canRead();
            else return ((File) getOb()).canRead();
        }

        public boolean canWrite() {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).canWrite();
            else return ((File) getOb()).canWrite();
        }

        public String getCanonicalPath() throws IOException {
            if (getOb() instanceof DocumentFile) return ((DocumentFile) getOb()).getUri().getPath();
            else return ((File) getOb()).getCanonicalPath();
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
            return FileUtil.convertDocumentFileToGen(FileUtil.getDocumentFileFromFileScopedStorage(f, f.getPath()));
        } else {
            return FileUtil.convertFileToGen(f);
        }
    }
}