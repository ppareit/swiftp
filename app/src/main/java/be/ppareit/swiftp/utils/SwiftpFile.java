package be.ppareit.swiftp.utils;

import android.content.Context;

import androidx.documentfile.provider.DocumentFile;

import org.jetbrains.annotations.Nullable;

import java.io.File;

import be.ppareit.swiftp.server.FtpCmd;

public class SwiftpFile {

    File f;
    DocumentFile df;

    public SwiftpFile(File f) {
        this.f = f;
    }

    public SwiftpFile(DocumentFile df) {
        this.df = df;
    }

    public boolean isFile() {
        return f != null;
    }

    public boolean isDocumentFile() {
        return df != null;
    }

    public String getPath() {
        if (f != null) return f.getPath();
        // if (df != null) todo can use URI here
        return "";
    }

    public boolean isDirectory() {
        if (f != null) return f.isDirectory();
        return false;
    }

    public boolean delete(Context c) {
        if (f != null) return FileUtil.deleteFile(f, c);
        if (df != null) return df.delete();
        return false;
    }

    public boolean exists() {
        if (f != null) return f.exists();
        if (df != null) return df.exists();
        return false;
    }

    public boolean violatesChroot(FtpCmd ftpCmd, @Nullable String path) {
        if (f != null) return ftpCmd.violatesChroot(f);
        if (df != null) return ftpCmd.violatesChroot(df, path);
        return false;
    }

    /*
    * Helper while the code gets changed
    * */
    public File getFile() {
        if (f != null) return f;
        return null;
    }

    /*
     * Helper while the code gets changed
     * */
    public DocumentFile getDocumentFile() {
        if (df != null) return df;
        return null;
    }
}
