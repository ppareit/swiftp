package be.ppareit.swiftp.server;

import androidx.documentfile.provider.DocumentFile;

import net.vrallev.android.cat.Cat;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.utils.FileUtil;


/**
 * CmdHASH provides a method to verify the integrity of a transferred file or to compare two files
 * files against each other without transferring them first. See draft-ietf-ftpext2-hash-03 in
 * the documentation.
 */
public class CmdHASH extends FtpCmd implements Runnable {
    private String input;

    public CmdHASH(SessionThread sessionThread, String input) {
        super(sessionThread);
        this.input = input;
    }

    @Override
    public void run() {
        Cat.d("HASH executing");
        String param = getParameter(input);
        File fileToHash;
        String errString = null;

        mainblock:
        {
            fileToHash = inputPathToChrootedFile(sessionThread.getChrootDir(),
                    sessionThread.getWorkingDir(), param);

            FileUtil.Gen gen = FileUtil.createGenFromFile(fileToHash);
            final boolean isDocumentFile = gen.getOb() instanceof DocumentFile;
            final boolean isFile = !isDocumentFile;

            if (isFile && violatesChroot((File) gen.getOb())
                    || isDocumentFile && violatesChroot((DocumentFile) gen.getOb())) {
                errString = "550 Invalid name or chroot violation\r\n";
                break mainblock;
            } else if (gen.isDirectory()) {
                Cat.d("Ignoring HASH for directory");
                errString = "553 Can't HASH a directory\r\n";
                break mainblock;
            } else if (!gen.exists()) {
                Cat.d("Can't HASH nonexistent file: " + gen.getAbsolutePath());
                errString = "550 File does not exist\r\n";
                break mainblock;
            } else if (!gen.canRead()) {
                Cat.i("Failed HASH permission (canRead() is false)");
                errString = "556 No read permissions\r\n";
                break mainblock;
            }

            FileInputStream in = null;
            InputStream is = null;
            try {
                String algorithm = sessionThread.getHashingAlgorithm();
                MessageDigest md = MessageDigest.getInstance(algorithm);
                byte[] buffer = new byte[SessionThread.DATA_CHUNK_SIZE];
                if (isFile) in = new FileInputStream((File) gen.getOb());
                else
                    is = App.getAppContext().getContentResolver().openInputStream(((DocumentFile) gen.getOb()).getUri());

                long offset = 0L;
                long endPosition = gen.length() - 1;
                if (sessionThread.offset >= 0) {
                    offset = sessionThread.offset;
                    if (offset <= sessionThread.endPosition
                            && sessionThread.endPosition <= gen.length() - 1) {
                        endPosition = sessionThread.endPosition;
                    }
                }

                // This is not a range but length (Range 0-0 would still read 0th byte), so +1
                long bytesToRead = endPosition - offset + 1;
                int bytesRead;
                if (isFile) in.skip(offset);
                else is.skip(offset);
                while ((bytesRead = isFile ? in.read(buffer) : is.read(buffer)) != -1) {
                    if (bytesRead > bytesToRead) {
                        md.update(buffer, 0, (int) bytesToRead);
                        break;
                    }
                    md.update(buffer, 0, bytesRead);
                    bytesToRead -= bytesRead;
                }

                byte[] hash = md.digest();
                StringBuilder hexString = new StringBuilder();
                for (byte b : hash) {
                    hexString.append(String.format("%02x", b));
                }

                String response = "213 " + algorithm + " " + offset + "-" + endPosition
                        + " " + hexString.toString() + " " + param + "\r\n";
                sessionThread.writeString(response);
            } catch (FileNotFoundException e) {
                errString = "550 File not found\r\n";
                break mainblock;
            } catch (IOException e) {
                errString = "425 Network error\r\n";
                break mainblock;
            } catch (NoSuchAlgorithmException e) {
                errString = "550 Unknown hashing algorithm\r\n";
                break mainblock;
            } finally {
                try {
                    if (in != null) in.close();
                } catch (IOException ignore) {
                }
                try {
                    if (is != null) is.close();
                } catch (IOException ignore) {
                }
            }
        }
        if (errString != null) {
            sessionThread.writeString(errString);
        }
        Cat.d("HASH done");
    }
}