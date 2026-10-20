// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/** Keeps SAF moves from copying or deleting a source while an upload writes to it. */
public final class SafAccess {
    private static final ReentrantReadWriteLock access = new ReentrantReadWriteLock(true);

    private SafAccess() {}

    public static Lease upload() {
        return new Lease(access.readLock());
    }

    public static Lease move() {
        return new Lease(access.writeLock());
    }

    public static final class Lease implements AutoCloseable {
        private final Lock lock;

        private Lease(Lock lock) {
            this.lock = lock;
            lock.lock();
        }

        @Override
        public void close() {
            lock.unlock();
        }
    }
}
