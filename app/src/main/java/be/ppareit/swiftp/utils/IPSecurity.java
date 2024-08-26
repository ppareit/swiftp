package be.ppareit.swiftp.utils;

import android.util.ArraySet;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import be.ppareit.swiftp.FsSettings;

public class IPSecurity {

    private static final boolean IS_VALID = true;
    private static final boolean IS_INVALID = false;
    private static final int VALID = 0;
    private static final int INVALID = 1;

    private static final ConcurrentHashMap<String, Integer> ipFailCount = new ConcurrentHashMap<>();

    private static Set<String> ipList;
    private static Set<String> allowList;
    private static Set<String> failList;

    static Logging logging = new Logging();

    public IPSecurity() {
    }

    public static void putIPFail(String ip) {
        Integer count = 1;
        if (ipFailCount.containsKey(ip)) {
            count = ipFailCount.get(ip);
            if (count == null) count = 1;
            else count++;
        }
        ipFailCount.put(ip, count);
    }

    public static Integer getIPFail(String ip) {
        if (ipFailCount.get(ip) != null) return ipFailCount.get(ip);
        return 0;
    }

    public static boolean isIPValid(String ip) {

        // IP enhancement settings
        final boolean isDenyUntilAllowed = FsSettings.isDenyUntilAllowed();
        final boolean isDenyOnFailedLogins = FsSettings.isDenyOnFailedLogins();

        if (!isDenyUntilAllowed
                && !isDenyOnFailedLogins) {
            // IP security enhancement settings are all disabled.
            logging.appendLog("IP connected" + ip);
            return true;
        }

        // Bring up IP lists
        ipList = FsSettings.getIPList();
        allowList = FsSettings.getAllowList();
        failList = FsSettings.getFailList();

        logIP(ip);

        int result = shouldDenyUntilAllowed(isDenyUntilAllowed, ip);
        if (result == INVALID) return IS_INVALID;
        if (result == VALID) return IS_VALID;

        result = shouldDenyOnFailedLogin(isDenyOnFailedLogins, ip);
        if (result == INVALID) return IS_INVALID;
        if (result == VALID) return IS_VALID;

        return IS_INVALID;
    }

    private static void logIP(String ip) {
        if (!ipList.contains(ip)) {
            logging.appendLog("New IP connected" + ip);
            Set<String> newList = new ArraySet<>();
            newList.add(ip);
            newList.addAll(ipList);
            FsSettings.putIPList(newList);
        } else {
            logging.appendLog("Existing IP connected" + ip);
        }
    }

    private static int shouldDenyUntilAllowed(boolean isDenyUntilAllowed, String ip) {
        if (isDenyUntilAllowed) {
            if (allowList.contains(ip)) {
                return VALID;
            } else {
                // deal with * use eg 000.111.* (helpful for mobile connections)
                for (String s : allowList) {
                    if (s.contains("*")) {
                        String aIP = "";
                        if (!s.startsWith("*"))
                            aIP = s.substring(0, s.indexOf("*") - 1);
                        // else... not allowing * at start.
                        if (!aIP.isEmpty()) { // not allowing empty
                            if (ip.startsWith(aIP)) {
                                return VALID;
                            }
                        }
                    }
                }
                logging.appendLog("IP denied entry " + ip);
                return INVALID;
            }
        }
        return -1;
    }

    private static int shouldDenyOnFailedLogin(boolean isDenyOnFailedLogins, String ip) {
        if (isDenyOnFailedLogins) {
            int count = 0;
            if (failList.contains(ip)) count = 3;
            count += getIPFail(ip);
            if (count <= 3 || allowList.contains(ip)) {
                return VALID;
            } else {
                if (!failList.contains(ip)) {
                    Set<String> newList = new ArraySet<>();
                    newList.add(ip);
                    newList.addAll(failList);
                    FsSettings.putFailList(newList);
                }
                logging.appendLog("IP denied. Too many failures." + ip);
                return INVALID;
            }
        }
        return -1;
    }
}
