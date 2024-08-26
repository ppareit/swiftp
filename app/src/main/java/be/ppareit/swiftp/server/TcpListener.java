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
import java.net.SocketException;

import android.util.Log;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.utils.IPSecurity;
import be.ppareit.swiftp.utils.Logging;

public class TcpListener extends Thread {
    private static final String TAG = TcpListener.class.getSimpleName();

    ServerSocket listenSocket;
    SSLServerSocket listenSSLSocket;
    FsService ftpServerService;

    // Avoid forever but allow Exceptions, especially with TLS, as Exceptions != should exit.
    private final int LOOP_EXCEPTION_CONTROL_MAX = 50;
    private int loopExControlPlain = 0;
    private int loopExControlTLS = 0;

    private final Logging logging = new Logging();

    public TcpListener(ServerSocket listenSocket, FsService ftpServerService, SSLServerSocket sslServerSocket) {
        this.listenSSLSocket = sslServerSocket;
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
        listenPlainSocket();
        listenSSLSocket();
    }

    private void listenPlainSocket() {
        if (listenSocket == null) return;
        if (FsSettings.isImplicitOnly()) return;

        new Thread(() -> {
            try {
                Socket clientSocket;
                while (true) {
                    try {
                        clientSocket = listenSocket.accept(); // blocks loop until next connection
                        logging.appendLog("Plain/Explicit port connected...");
                    } catch (Exception e) {
                        loopExControlPlain++;
                        // call to close() on server off causes SocketException here with "Socket closed"
                        final String msg = e.getMessage();
                        if (msg != null && msg.contains("Socket closed")) {
                            if (ftpServerService.isConnWakelockRunning()) {
                                ftpServerService.releaseWakelocks();
                            }
                            return;
                        }
                        // Simple retry and fail back to next catch or continue on success
                        clientSocket = listenSocket.accept();
                    }

                    if (IPSecurity.isIPValid(clientSocket.getInetAddress().toString())) {
                        spawnSession(clientSocket, null);
                        loopExControlPlain = 0;
                    } else {
                        Log.d(TAG, "IP denied access.");
                        logging.appendLog("IP denied access.");
                        clientSocket.close();
                    }

                    if (loopExControlPlain > LOOP_EXCEPTION_CONTROL_MAX) {
                        loopExControlPlain = 0;
                        if (ftpServerService.isConnWakelockRunning()) {
                            ftpServerService.releaseWakelocks();
                        }
                        break;
                    }
                }
            } catch (Exception e) {
                loopExControlPlain++;
                Log.d(TAG, "Exception in TcpListener");
                if (ftpServerService.isConnWakelockRunning()) {
                    ftpServerService.releaseWakelocks();
                }
            }
        }).start();
    }

    private void listenSSLSocket() {
        if (listenSocket == null) return;
        if (!FsSettings.isImplicitUsed()) return;

        new Thread(() -> {
            SSLSocket clientSSLSocket;
            //  might want anyway to eliminate a port from listening.
            while (true) {
                try {
                    try {
                        clientSSLSocket = (SSLSocket) listenSSLSocket.accept();
                        logging.appendLog("FTPS implicit port connected");
                    } catch (Exception e) {
                        loopExControlTLS++;
                        // call to close() on server off causes SocketException here with "Socket closed"
                        final String msg = e.getMessage();
                        if (msg != null && msg.contains("Socket closed")) {
                            if (ftpServerService.isConnWakelockRunning()) {
                                ftpServerService.releaseWakelocks();
                            }
                            return;
                        }
                        // Simple retry and fail back to next catch or continue on success
                        clientSSLSocket = (SSLSocket) listenSSLSocket.accept();
                    }

                    if (IPSecurity.isIPValid(clientSSLSocket.getInetAddress().toString())) {
                        Log.i(TAG, "New connection, spawned thread");
                        final SSLSocket finalClientSSLSocket = clientSSLSocket;
                        clientSSLSocket.addHandshakeCompletedListener(event -> {
                            Log.i(TAG, "Handshake completed");
                            logging.appendLog("Handshake completed");
                            changeSocketTimeout(finalClientSSLSocket, 0);
                            spawnSession(null, finalClientSSLSocket);
                            loopExControlTLS = 0;
                        });
                        Log.i(TAG, "Begin FTPS handshake");
                        logging.appendLog("Begin FTPS handshake");
                        // Incorrect setup will cause startHandshake() to timeout
                        clientSSLSocket.setTcpNoDelay(true);
                        changeSocketTimeout(clientSSLSocket, 30000);
                        clientSSLSocket.startHandshake();
                    } else {
                        Log.d(TAG, "IP denied access.");
                        logging.appendLog("IP denied access.");
                        clientSSLSocket.close();
                    }

                } catch (Exception e) {
                    loopExControlTLS++;
                    // Samsung Files implicit reaches here from it not liking the handshake.
                    // Certs can cause handshake failures and end up here.
                    // Handshake timeout will end up here.
                    // Clients can fail the handshake.
                    Log.d(TAG, "Exception in TcpListener");
                    if (ftpServerService.isConnWakelockRunning()) {
                        ftpServerService.releaseWakelocks();
                    }
                }
                if (loopExControlTLS > LOOP_EXCEPTION_CONTROL_MAX) {
                    loopExControlTLS = 0;
                    if (ftpServerService.isConnWakelockRunning()) {
                        ftpServerService.releaseWakelocks();
                    }
                    break;
                }
            }
        }).start();
    }

    private void changeSocketTimeout(SSLSocket socket, int time) {
        // Timeout is useful for handshake failures.
        // Timeout must be 0 on handshake completion(!) or else randomly quits with enough work.
        try {
            socket.setSoTimeout(time);
        } catch (SocketException e) {
            throw new RuntimeException(e);
        }
    }

    private void spawnSession(Socket clientSocket, SSLSocket sslClientSocket) {
        Log.i(TAG, "New connection, spawned thread");
        ftpServerService.createConnWakeLock();
        SessionThread newSession = new SessionThread(clientSocket, new LocalDataSocket(), sslClientSocket);
        newSession.start();
        ftpServerService.registerSessionThread(newSession);
    }
}
