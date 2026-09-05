/*
Copyright 2009 David Revell

This file is part of SwiFTP.

SwiFTP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SwiFTP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SwiFTP.  If not, see <http://www.gnu.org/licenses/>.
 */

package be.ppareit.swiftp.server;

import android.util.Log;

import java.io.File;
import java.lang.reflect.Constructor;
import java.net.InetAddress;

import be.ppareit.swiftp.Util;
import be.ppareit.swiftp.utils.AllowedFolders;
import be.ppareit.swiftp.utils.Logging;

public abstract class FtpCmd implements Runnable {
    private static final String TAG = FtpCmd.class.getSimpleName();

    protected SessionThread sessionThread;

    protected static CmdMap[] cmdClasses = { new CmdMap("SYST", CmdSYST.class),
            new CmdMap("USER", CmdUSER.class), new CmdMap("PASS", CmdPASS.class),
            new CmdMap("TYPE", CmdTYPE.class), new CmdMap("CWD", CmdCWD.class),
            new CmdMap("PWD", CmdPWD.class), new CmdMap("LIST", CmdLIST.class),
            new CmdMap("PASV", CmdPASV.class), new CmdMap("RETR", CmdRETR.class),
            new CmdMap("NLST", CmdNLST.class), new CmdMap("NOOP", CmdNOOP.class),
            new CmdMap("STOR", CmdSTOR.class), new CmdMap("DELE", CmdDELE.class),
            new CmdMap("RNFR", CmdRNFR.class), new CmdMap("RNTO", CmdRNTO.class),
            new CmdMap("RMD", CmdRMD.class), new CmdMap("MKD", CmdMKD.class),
            new CmdMap("OPTS", CmdOPTS.class), new CmdMap("PORT", CmdPORT.class),
            new CmdMap("QUIT", CmdQUIT.class), new CmdMap("FEAT", CmdFEAT.class),
            new CmdMap("SIZE", CmdSIZE.class), new CmdMap("CDUP", CmdCDUP.class),
            new CmdMap("APPE", CmdAPPE.class), new CmdMap("XCUP", CmdCDUP.class), // synonym
            new CmdMap("XPWD", CmdPWD.class), // synonym
            new CmdMap("XMKD", CmdMKD.class), // synonym
            new CmdMap("XRMD", CmdRMD.class), // synonym
            new CmdMap("MDTM", CmdMDTM.class), //
            new CmdMap("MFMT", CmdMFMT.class), //
            new CmdMap("REST", CmdREST.class), //
            new CmdMap("SITE", CmdSITE.class), //
            new CmdMap("MLST", CmdMLST.class), //
            new CmdMap("MLSD", CmdMLSD.class), //
            new CmdMap("HASH", CmdHASH.class),
            new CmdMap("RANG", CmdRANG.class),
            new CmdMap("AUTH", CmdAUTH.class),
            new CmdMap("PROT", CmdPROT.class),
            new CmdMap("PBSZ", CmdPBSZ.class),
            new CmdMap("EPRT", CmdEPRT.class),
            new CmdMap("EPSV", CmdEPSV.class)
    };

    private static Class<?>[] allowedCmdsWhileAnonymous = { CmdUSER.class, CmdPASS.class, //
            CmdCWD.class, CmdLIST.class, CmdMDTM.class, CmdNLST.class, CmdPASV.class, //
            CmdPWD.class, CmdQUIT.class, CmdRETR.class, CmdSIZE.class, CmdTYPE.class, //
            CmdCDUP.class, CmdNOOP.class, CmdSYST.class, CmdPORT.class, //
            CmdMLST.class, CmdMLSD.class, CmdHASH.class, CmdRANG.class, CmdAUTH.class, //
            CmdPROT.class, CmdPBSZ.class, CmdFEAT.class, //
            CmdMLST.class, CmdMLSD.class, CmdHASH.class, CmdRANG.class, //
            CmdEPRT.class, CmdEPSV.class //
    };

    public FtpCmd(SessionThread sessionThread) {
        this.sessionThread = sessionThread;
    }

    /** Where this command's session has a login accepted or refused. */
    protected Authenticator authenticator() {
        return sessionThread.authenticator();
    }

    /** The settings this command's session answers from. */
    protected Settings settings() {
        return sessionThread.settings();
    }

    /** The connection log for this command's session. */
    protected Logging logging() {
        return sessionThread.getLogging();
    }

    /**
     * Whether an active data connection may be opened to this address.
     *
     * RFC 2577 section 3: only the host that issued the command, so the server cannot
     * be used to reach a third host on the client's behalf (the FTP bounce attack).
     * Comparing InetAddress ignores the IPv6 scope id, which is what we want: the
     * client names a link local address without one, we hold the same address with
     * the interface it arrived on.
     */
    protected boolean isControlPeer(InetAddress dest) {
        InetAddress peer = sessionThread.getControlPeerAddress();
        return peer != null && peer.equals(dest);
    }

    @Override
    abstract public void run();

    protected static void dispatchCommand(SessionThread session, String inputString) {
        String[] strings = inputString.split(" ");
        String unrecognizedCmdMsg = "502 Command not recognized\r\n";
        if (strings == null) {
            // There was some egregious sort of parsing error
            String errString = "502 Command parse error\r\n";
            Log.d(TAG, errString);
            session.writeString(errString);
            return;
        }
        if (strings.length < 1) {
            Log.d(TAG, "No strings parsed");
            session.writeString(unrecognizedCmdMsg);
            return;
        }
        String verb = strings[0];
        if (verb.length() < 1) {
            Log.i(TAG, "Invalid command verb");
            session.writeString(unrecognizedCmdMsg);
            return;
        }
        FtpCmd cmdInstance = null;
        verb = verb.trim();
        verb = verb.toUpperCase();
        for (int i = 0; i < cmdClasses.length; i++) {

            if (cmdClasses[i].getName().equals(verb)) {
                // We found the correct command. We retrieve the corresponding
                // Class object, get the Constructor object for that Class, and
                // and use that Constructor to instantiate the correct FtpCmd
                // subclass. Yes, I'm serious.
                Constructor<? extends FtpCmd> constructor;
                try {
                    constructor = cmdClasses[i].getCommand().getConstructor(
                            new Class[] { SessionThread.class, String.class });
                } catch (NoSuchMethodException e) {
                    Log.e(TAG, "FtpCmd subclass lacks expected " + "constructor ");
                    return;
                }
                try {
                    cmdInstance = constructor.newInstance(new Object[] { session,
                            inputString });
                } catch (Exception e) {
                    Log.e(TAG, "Instance creation error on FtpCmd");
                    return;
                }
            }
        }
        if (cmdInstance == null) {
            // If we couldn't find a matching command,
            Log.d(TAG, "Ignoring unrecognized FTP verb: " + verb);
            session.writeString(unrecognizedCmdMsg);
            return;
        }

        // Used for FTPS encryption only setting.
        // Only affects explicit connection.
        final boolean forceAUTHTLS = session.settings().isEncryptionOnlyEnabled();
        final boolean socketNotEncrypted = session.getIsPlainSocket();
        if (forceAUTHTLS && socketNotEncrypted) {
            // When enabled, client has to use AUTH command and make the connection encrypted.
            // FEAT is how the client finds out that AUTH is what we are waiting for
            if (cmdInstance.getClass().equals(CmdAUTH.class)
                    || cmdInstance.getClass().equals(CmdFEAT.class)) cmdInstance.run();
            else session.writeString("530 Login first with AUTH, or QUIT\r\n");
            return;
        }

        if (session.isUserLoggedIn()) {
            cmdInstance.run();
        } else if (session.isAnonymouslyLoggedIn() == true) {
            boolean validCmd = false;
            for (Class<?> cl : allowedCmdsWhileAnonymous) {
                if (cmdInstance.getClass().equals(cl)) {
                    validCmd = true;
                    break;
                }
            }
            if (validCmd == true) {
                cmdInstance.run();
            } else {
                session.writeString("530 Guest user is not allowed to use that command\r\n");
            }
        } else if (cmdInstance.getClass().equals(CmdUSER.class)
                || cmdInstance.getClass().equals(CmdPASS.class)
                || cmdInstance.getClass().equals(CmdQUIT.class)
                // RFC 2389 section 3: FEAT has to be answerable before login, otherwise
                // a client cannot know whether AUTH TLS is on offer
                || cmdInstance.getClass().equals(CmdFEAT.class)
                || cmdInstance.getClass().equals(CmdAUTH.class)
                || cmdInstance.getClass().equals(CmdPROT.class)
                || cmdInstance.getClass().equals(CmdPBSZ.class)
                || cmdInstance.getClass().equals(CmdEPRT.class)
                || cmdInstance.getClass().equals(CmdEPSV.class)) {
            cmdInstance.run();
        } else {
            session.writeString("530 Login first with USER, PASS, AUTH, or QUIT\r\n");
        }
    }

    /**
     * An FTP parameter is that part of the input string that occurs after the first
     * space, including any subsequent spaces. Also, we want to chop off the trailing
     * '\r\n', if present.
     *
     * Some parameters shouldn't be logged or output (e.g. passwords), so the caller can
     * use silent==true in that case.
     */
    static public String getParameter(String input, boolean silent) {
        if (input == null) {
            return "";
        }
        int firstSpacePosition = input.indexOf(' ');
        if (firstSpacePosition == -1) {
            return "";
        }
        String retString = input.substring(firstSpacePosition + 1);

        // Remove trailing whitespace
        // todo: trailing whitespace may be significant, just remove \r\n
        retString = retString.replaceAll("\\s+$", "");

        // Fix: WinSCP synchronize with checksum reproduces a bad path here because of quotes.
        retString = retString.replaceAll("\"", "");

        if (!silent) {
            Log.d(TAG, "Parsed argument: " + retString);
        }
        return retString;
    }

    /**
     * A wrapper around getParameter, for when we don't want it to be silent.
     */
    static public String getParameter(String input) {
        return getParameter(input, false);
    }

    /**
     * Resolves a path the client sent: absolute ones against the chroot, relative ones against
     * {@code existingPrefix}, normally the working dir. Both live in the FTP namespace, so under
     * scoped storage the result is mapped back to a physical path. This does not enforce the
     * chroot; callers check the result with violatesChroot.
     */
    public static File inputPathToChrootedFile(final File chrootDir, final File existingPrefix, String ftpPath) {
        if (ftpPath == null) ftpPath = "";

        File namespacePrefix = existingPrefix;
        if (Util.useScopedStorage()) {
            try {
                final String virtualPrefix = AllowedFolders.virtualPathForPhysical(
                        existingPrefix.getCanonicalPath(), chrootDir.getCanonicalPath());
                if (virtualPrefix != null) namespacePrefix = new File(virtualPrefix);
            } catch (Exception ignored) {
                // Keep the physical prefix; later validation still enforces the chroot.
            }
        }

        final File path;
        if (ftpPath.startsWith(File.separator)) {
            // The command contained an absolute FTP path.
            path = new File(chrootDir, ftpPath);
        } else {
            path = new File(namespacePrefix, ftpPath);
        }

        if (!Util.useScopedStorage()) return path;
        try {
            final String physicalPath = AllowedFolders.physicalPathForVirtual(path.getCanonicalPath());
            return physicalPath == null ? path : new File(physicalPath);
        } catch (Exception e) {
            return path;
        }
    }

    /**
     * True when the path is the chroot itself, or something below it.
     */
    private static boolean isWithinChroot(String canonicalChroot, String canonicalPath) {
        if (canonicalPath.equals(canonicalChroot)) {
            return true;
        }
        if (canonicalChroot.endsWith(File.separator)) {
            // the filesystem root, which already carries its separator
            return canonicalPath.startsWith(canonicalChroot);
        }
        // the separator has to take part: a bare startsWith accepts every sibling whose
        // name merely begins with the chroot's, eg /sdcard/Sharefolder for /sdcard/Share
        return canonicalPath.startsWith(canonicalChroot + File.separator);
    }

    /**
     * The path as the client should see it, rooted at the chroot, or null when the path
     * lies outside the chroot and there is nothing sensible to report.
     */
    static String chrootRelativePath(String canonicalChroot, String canonicalPath) {
        if (!isWithinChroot(canonicalChroot, canonicalPath)) {
            return null;
        }
        // the filesystem root carries a separator that the visible path has to keep
        int prefix = canonicalChroot.endsWith(File.separator)
                ? canonicalChroot.length() - 1
                : canonicalChroot.length();
        String relative = canonicalPath.substring(prefix);
        return relative.isEmpty() ? File.separator : relative;
    }

    public boolean violatesChroot(File file) {
        try {
            // taking the canonical path as new devices have sdcard symbolic linked
            // for multi user support
            File chroot = sessionThread.getChrootDir();
            String canonicalChroot = chroot.getCanonicalPath();
            String canonicalPath = file.getCanonicalPath();
            if (!isWithinChroot(canonicalChroot, canonicalPath)) {
                Log.i(TAG, "Path violated folder restriction, denying");
                Log.d(TAG, "path: " + canonicalPath);
                Log.d(TAG, "chroot: " + chroot.toString());
                return true; // the path must be the chroot or below it
            }
            return false;
        } catch (Exception e) {
            Log.i(TAG, "Path canonicalization problem: " + e.toString());
            if (file != null) Log.i(TAG, "When checking file: " + file.getAbsolutePath()); // fix possible crash
            return true; // for security, assume violation
        }
    }
}
