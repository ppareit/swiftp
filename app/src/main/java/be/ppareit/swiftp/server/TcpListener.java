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

import java.net.ServerSocket;
import java.net.Socket;

import android.util.Log;
import be.ppareit.swiftp.FsService;

public class TcpListener extends Thread {
    private static final String TAG = TcpListener.class.getSimpleName();

    ServerSocket listenSocket;
    FsService ftpServerService;

    public TcpListener(ServerSocket listenSocket, FsService ftpServerService) {
        this.listenSocket = listenSocket;
        this.ftpServerService = ftpServerService;
    }

    public void quit() {
        try {
            listenSocket.close(); // if the TcpListener thread is blocked on accept,
                                  // closing the socket will raise an exception
        } catch (Exception e) {
            Log.d(TAG, "Exception closing TcpListener listenSocket");
        }
    }

    @Override
    public void run() {
        try {
            Socket clientSocket;
            while (true) {
                try {
                    clientSocket = listenSocket.accept(); // blocks loop until next connection
                } catch (Exception e) {
                    // call to close() on server off causes SocketException here with "Socket closed"
                    final String msg = e.getMessage();
                    if (msg != null && msg.contains("Socket closed")) {
                        if (ftpServerService.isConnWakelockRunning()) ftpServerService.releaseWakelocks();
                        return;
                    }
                    // Simple retry and fail back to next catch or continue on success
                    clientSocket = listenSocket.accept();
                }
                Log.i(TAG, "New connection, spawned thread");
                ftpServerService.createConnWakeLock();
                SessionThread newSession = new SessionThread(clientSocket, new LocalDataSocket());
                newSession.start();
                ftpServerService.registerSessionThread(newSession);
            }
        } catch (Exception e) {
            Log.d(TAG, "Exception in TcpListener");
            if (ftpServerService.isConnWakelockRunning()) ftpServerService.releaseWakelocks();
        }
    }
}
