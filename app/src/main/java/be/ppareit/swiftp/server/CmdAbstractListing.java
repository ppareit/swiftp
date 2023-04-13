/*
Copyright 2013 Pareit Pieter
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

/*
 * Since the FTP verbs LIST and NLST do very similar things related to listing
 * directory contents, the common tasks that they share have been factored
 * out into this abstract class. Both CmdLIST and CmdNLST inherit from this
 * class.
 */

package be.ppareit.swiftp.server;

import java.io.File;
import java.util.Arrays;
import java.util.Comparator;

import android.util.Log;

public abstract class CmdAbstractListing extends FtpCmd {
    // TODO: .class.getSimpleName() from abstract class?
    private static String TAG = "CmdAbstractListing";

    public CmdAbstractListing(SessionThread sessionThread, String input) {
        super(sessionThread);
    }

    abstract String makeLsString(File file);

    // Creates a directory listing by finding the contents of the directory,
    // calling makeLsString on each file, and concatenating the results.
    // Returns an error string if failure, returns null on success. May be
    // called by CmdLIST or CmdNLST, since they each override makeLsString
    // in a different way.
    public String listDirectory(StringBuilder response, File dir) {
        if (!dir.isDirectory()) {
            return "500 Internal error, listDirectory on non-directory\r\n";
        }
        Log.d(TAG, "Listing directory: " + dir.toString());

        return listEntries(response, dir);
    }

    private String listEntries(StringBuilder response, File dir) {
        // Get a listing of all files and directories in the path
        File[] entries = dir.listFiles();
        if (entries == null) {
            return "500 Couldn't list directory. Check config and mount status.\r\n";
        }
        Log.d(TAG, "Dir len " + entries.length);

        // Commented for performance. Not seeing any negative effect from not using. Client should do this anyway.
/*        try {
            Arrays.sort(entries, listingComparator);
        } catch (Exception e) {
            // once got a FC on this, seems it is possible to have a dir that
            // breaks the listing comparator (unable to reproduce)
            Log.e(TAG, "Unable to sort the listing: " + e.getMessage());
            // play for sure, and get back the entries
            entries = dir.listFiles();
        }

        if (entries == null) {
            return "500 Couldn't list directory. Check config and mount status.\r\n";
        }*/

        // 5,161 files being listed:
        // Old non-threaded time: 9 seconds
        // New threaded time: < 1 second
        final int cpuThreadCount = Runtime.getRuntime().availableProcessors();
        if (entries.length < cpuThreadCount) { // use cpu count as an absolute minimum
            buildResponseOldStyle(response, entries);
        } else {
            buildResponseThreaded(response, entries, cpuThreadCount);
        }

        return null;
    }

    private void buildResponseOldStyle(StringBuilder response, File[] entries) {
        for (File entry : entries) {
            String curLine = makeLsString(entry);
            if (curLine != null) {
                response.append(curLine);
            }
        }
    }

    private void buildResponseThreaded(StringBuilder response, File[] entries, final int cpuThreadCount) {
        // Prep for threading per core/thread count
        // More than 2 threads should only help more as the entries count increase
        Thread[] threads = new Thread[cpuThreadCount];
        StringBuilder[] responses = new StringBuilder[cpuThreadCount];
        final int totalCount = entries.length;
        final int splitCount = totalCount / cpuThreadCount;
        int[] startLoc = new int[cpuThreadCount];
        int[] endLoc = new int[cpuThreadCount]; // eg 270 is total count 271 since starting at 0
        for (int i = 0; i < cpuThreadCount; i++) {
            startLoc[i] = splitCount * i;
            if (i > 0) startLoc[i]++; // add one so they don't start where the other ends
            endLoc[i] = i == 0 ? splitCount : splitCount * (i + 1);
            if (i == cpuThreadCount - 1) {
                if (endLoc[i] != totalCount - 1)
                    endLoc[i] = totalCount - 1; // sometimes off
            }
            responses[i] = new StringBuilder();
        }

        // Split off onto the cores/threads of the device for more performance here
        for (int i = 0; i < cpuThreadCount; i++) {
            threads[i] = dynamicThreadSplitter(entries, responses, i, startLoc, endLoc);
            threads[i].start();
        }

        // Need to wait until all are finished starting with first running to last running
        dynamicThreadJoiner(threads);

        // Need to include results from first to last to keep them in the proper order
        response.append(dynamicResponseJoiner(responses));
    }

    private Thread dynamicThreadSplitter(File[] entries, StringBuilder[] response, final int loc, int[] startLoc,
                                         int[] endLoc) {
        return new Thread(() -> {
            for (int i = startLoc[loc]; i <= endLoc[loc]; i++) { // Needs to start at next of prev thread
                String curLine = makeLsString(entries[i]);
                if (curLine != null) {
                    response[loc].append(curLine);
                }
            }
        });
    }

    private void dynamicThreadJoiner(Thread[] t) {
        for (Thread thread : t) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            } catch (NullPointerException e) {
                // just continue marching on
            }
        }
    }

    private StringBuilder dynamicResponseJoiner(StringBuilder[] sb) {
        StringBuilder result = new StringBuilder();
        for (StringBuilder each : sb) {
            result.append(each);
        }
        return result;
    }

    // Send the directory listing over the data socket. Used by CmdLIST and CmdNLST.
    // Returns an error string on failure, or returns null if successful.
    protected String sendListing(String listing) {
        if (sessionThread.openDataSocket()) {
            Log.d(TAG, "LIST/NLST done making socket");
        } else {
            sessionThread.closeDataSocket();
            return "425 Error opening data socket\r\n";
        }
        String mode = sessionThread.isBinaryMode() ? "BINARY" : "ASCII";
        sessionThread.writeString("150 Opening " + mode
                + " mode data connection for file list\r\n");
        Log.d(TAG, "Sent code 150, sending listing string now");
        if (!sessionThread.sendViaDataSocket(listing)) {
            Log.d(TAG, "sendViaDataSocket failure");
            sessionThread.closeDataSocket();
            return "426 Data socket or network error\r\n";
        }
        sessionThread.closeDataSocket();
        Log.d(TAG, "Listing sendViaDataSocket success");
        sessionThread.writeString("226 Data transmission OK\r\n");
        return null;
    }

    /**
     * Comparator to sort file listings. Sorts directories before files, sorts
     * alphabetical ignoring case
     */
    static final Comparator<File> listingComparator = (lhs, rhs) -> {
        if (lhs.isDirectory() && rhs.isFile()) {
            return -1;
        } else if (lhs.isFile() && rhs.isDirectory()) {
            return 1;
        } else {
            return lhs.getName().compareToIgnoreCase(rhs.getName());
        }
    };
}
