//package org.swiftp;
//
//import java.io.FileOutputStream;
//import java.io.IOException;
//import java.util.Queue;
//import java.util.concurrent.Semaphore;
//
//import android.util.Log;
//
//public class DedicatedWriter extends Thread {
//	boolean shouldExit = false;
//	boolean errorFlag = false;
//	FileOutputStream out;
//	MyLog myLog = new MyLog(DedicatedWriter.class.getName());
//	byte[] curBuffer = null, nextBuffer = null;
//	int curBufferSize = 0, nextBufferSize = 0;
//	//ReentrantLock lock = new ReentrantLock();
//	//Semaphore sem = new Semaphore(1);
//	int maxBufferWaitMs = 10000;
//	static final int enqueueRetryMs = 1000;
//	//int inactiveWaitMs = 5;
//	Thread waitingThread = null;
//	
//	public DedicatedWriter(FileOutputStream out) {
//		this.out = out;
//		/*try {
//			sem.acquire();
//		} catch (InterruptedException e) {
//			// cannot happen
//			myLog.l(Log.ERROR, "DedicatedWriter init semaphore");
//		}*/
//	}
//
//	synchronized void waitForNextBuffer() {
//		while(nextBuffer == null && !shouldExit) {
//			try {
//				wait();
//			} catch (InterruptedException e) {
//				Log.d("Writer", "Writer interrupted (good)");
//			}
//		}
//		if(shouldExit) {
//			return;
//		}
//		curBufferSize = nextBufferSize;
//		curBuffer = nextBuffer;
//		nextBufferSize = 0;
//		nextBuffer = null;
//		//Log.d("Writer", "Writer notifyAll");
//		notifyAll();
//	}
//	
//	public void run() {
//		Thread.currentThread().setPriority(Thread.MAX_PRIORITY);
//		int newPriority = Thread.currentThread().getPriority();
//		myLog.l(Log.DEBUG, "New DedicatedWriter prio: " + newPriority);
//		
//		while(!shouldExit) {
//			waitForNextBuffer();
//			if(shouldExit) {
//				break;
//			}
//			try {
//				out.write(curBuffer, 0, curBufferSize);
//			} catch (IOException e) {
//				//Log.d("Writer", "Exception while writing");
//				errorFlag = true;
//				shouldExit = true;
//			}
//		}
//		myLog.l(Log.DEBUG, "DedicatedWriter exiting");
//		/*while(!shouldExit) {
//			while(nextBuffer == null || shouldExit) {
//				//myLog.l(Log.DEBUG, "Nothing to write, waiting");
//				if(shouldExit) {
//					break;
//				}
//				try {
//					Thread.sleep(inactiveWaitMs);
//				} catch (InterruptedException e) {}
//			}
//			if(nextBuffer != null) {
//				curBufferSize = nextBufferSize;
//				curBuffer = nextBuffer;
//				nextBufferSize = 0;
//				nextBuffer = null;
//				try {
//					out.write(curBuffer, 0, curBufferSize);
//				} catch (IOException e) {
//					myLog.l(Log.INFO, "Exception while writing");
//					errorFlag = true;
//				}
//			}
//		}*/
//	}
//	
//	public void exit() {
//		shouldExit = true;
//	}
//	
//	synchronized public void enqueueBuffer(byte[] inBuffer, int size) throws IOException {
//		// Just in case the writer thread is waiting incorrectly, this is safe
//		
//		while(nextBuffer != null && !shouldExit) {
//			try {
//				wait();
//			} catch(InterruptedException e) {}
//		}
//		
//		nextBufferSize = size;
//		nextBuffer = inBuffer;
//		//Log.d("Writer", "Queuer notifyAll");
//		notifyAll();
//
//		//int waitedMs = 0;
//		/*while(nextBuffer != null) {
//			if(waitedMs > maxBufferWaitMs) {
//				throw new IOException("DedicatedWriter stuck waiting");
//			}
//			try {
//				Thread.sleep(enqueueRetryMs);
//			} catch(InterruptedException e) {}
//			
//			waitedMs += enqueueRetryMs;
//		}
//		nextBufferSize = size;
//		nextBuffer = inBuffer;*/
//	}
//	
//	public boolean checkErrorFlag() {
//		return errorFlag;
//	}
//
//}
