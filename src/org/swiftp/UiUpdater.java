package org.swiftp;

import java.util.ArrayList;
import java.util.List;

import android.os.Handler;
import android.util.Log;

public class UiUpdater {
	protected static MyLog myLog = new MyLog("UiUpdater");
	protected static List<Handler> clients = new ArrayList<Handler>();
	
	static void registerClient(Handler client) {
		if(!clients.contains(client)) {
			clients.add(client);
		}
	}
	
	static void unregisterClient(Handler client) {
		clients.remove(client);
	}
	
	static void updateClients() {
		myLog.l(Log.DEBUG, "UI update");
		for (Handler client : clients) {
			client.sendEmptyMessage(0);
		}
	}
}
