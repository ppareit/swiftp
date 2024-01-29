package be.ppareit.swiftp.server;

import androidx.annotation.NonNull;

import java.io.File;

import be.ppareit.swiftp.FsSettings;

public class FtpUser {

    final private String mUsername;
    final private String mPassword;
    final private String mChroot;
    final private String mUriString;

    public FtpUser(@NonNull String username, @NonNull String password, @NonNull String chroot, String uriString) {
        mUsername = username;
        mPassword = password;

        final File rootPath = new File(chroot);
        mChroot = rootPath.isDirectory() ? chroot : FsSettings.getDefaultChrootDir().getPath();
        mUriString = uriString;
    }

    public String getUsername() {
        return mUsername;
    }

    public String getPassword() {
        return mPassword;
    }

    public String getChroot() {
        return mChroot;
    }

    public String getUriString() {
        return mUriString;
    }
}
