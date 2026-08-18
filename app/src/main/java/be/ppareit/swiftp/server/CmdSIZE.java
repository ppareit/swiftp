// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.server;

import java.io.File;

import android.util.Log;

import be.ppareit.swiftp.utils.FileUtil;

public class CmdSIZE extends FtpCmd {
    private static final String TAG = CmdSIZE.class.getSimpleName();

    protected String input;

    public CmdSIZE(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Log.d(TAG, "SIZE executing");
        String errString = null;
        String param = getParameter(input);
        long size = 0;
        mainblock: {
            File target = inputPathToChrootedFile(sessionThread.getChrootDir(),
                    sessionThread.getWorkingDir(), param);
            if (violatesChroot(target)) {
                errString = "550 SIZE target violates chroot\r\n";
                break mainblock;
            }
            // Under scoped storage the plain File cannot be read and would report 0
            FileUtil.Gen gen = FileUtil.createGenFromFile(target);
            if (!gen.exists()) {
                errString = "550 Cannot get the SIZE of nonexistent object\r\n";
                Log.d(TAG, "Failed getting size of: " + target.getAbsolutePath());
                break mainblock;
            }
            if (!gen.isFile()) {
                errString = "550 Cannot get the size of a non-file\r\n";
                break mainblock;
            }
            size = gen.length();
        }
        if (errString != null) {
            sessionThread.writeString(errString);
        } else {
            sessionThread.writeString("213 " + size + "\r\n");
        }
        Log.d(TAG, "SIZE complete");
    }

}
