package be.ppareit.swiftp.utils;

import java.util.concurrent.atomic.AtomicInteger;

public class AnonymousLimit {
    static AtomicInteger anonUsersConnected = new AtomicInteger(0);

    public static int incrementAndGet() {
        return anonUsersConnected.incrementAndGet();
    }

    public static void decrement() {
        if (anonUsersConnected.get() >= 1) anonUsersConnected.decrementAndGet();
    }
}