// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.server;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import java.io.File;

import be.ppareit.swiftp.FsSettings;

public class FtpUser {

    @SerializedName(value = "username", alternate = "mUsername")
    final private String mUsername;
    @SerializedName(value = "password", alternate = "mPassword")
    final private String mPassword;
    @SerializedName(value = "chroot", alternate = "mChroot")
    final private String mChroot;
    @SerializedName(value = "uriString", alternate = "mUriString")
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
