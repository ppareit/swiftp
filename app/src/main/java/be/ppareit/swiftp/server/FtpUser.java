package be.ppareit.swiftp.server;

import be.ppareit.swiftp.FsSettings;

public class FtpUser {

    private String username, password, chroot;

    public FtpUser(String username, String password, String chroot) {
        this.username = username;
        this.password = password;
        this.chroot = chroot.equals("null") ? FsSettings.getDefaultChrootDir().getPath() : chroot;
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public String getChroot() {
        return chroot;
    }
}
