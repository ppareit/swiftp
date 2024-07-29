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

import java.io.IOException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.Arrays;
import java.util.Random;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.utils.FTPSSockets;
import be.ppareit.swiftp.utils.Logging;

public class LocalDataSocket {
    private static final String TAG = LocalDataSocket.class.getSimpleName();

    private static final int SO_TIMEOUT_MS = 30000; // socket timeout millis
    public static final int TCP_CONNECTION_BACKLOG = 5;

    // Listener socket used for PASV mode
    ServerSocket server = null;
    SSLServerSocket sslServer = null;
    // Remote IP & port information used for PORT mode
    private InetAddress remoteAddress = null;
    private int remotePort;
    private Inet6Address remote6Address = null;
    private int remote6Port;
    private final Logging logging = new Logging();

    private final FTPSSockets ftpsSockets = new FTPSSockets();

    public LocalDataSocket() {
        clearState();
    }

    /**
     * Clears the state of this object, as if no pasv() or port() had occurred. All
     * sockets are closed.
     */
    private void clearState() {
        if (server != null) {
            try {
                server.close();
            } catch (IOException e) {
                //
            }
            server = null;
        }
        if (sslServer != null) {
            try {
                sslServer.close();
            } catch (IOException e) {
                //
            }
            sslServer = null;
        }
        remoteAddress = null;
        remotePort = 0;
        remote6Address = null;
        remote6Port = 0;
        Log.d(TAG, "State cleared");
    }

    public int onPasv(boolean ssl) {
        clearState();
        try {
            // Listen on any port (port parameter 0)
            if (ssl) {
                sslServer = ftpsSockets.createSSLServerSocket();
                Log.d(TAG, "Data socket pasv() listen successful");
                return sslServer.getLocalPort();
            }
            server = new ServerSocket(getNewPort(), TCP_CONNECTION_BACKLOG);
            Log.d(TAG, "Data socket pasv() listen successful");
            return server.getLocalPort();
        } catch (Exception e) {
            Log.e(TAG, "Data socket creation error");
            clearState();
            return 0;
        }
    }

    public int onEpsvPlain(InetAddress address) {
        try {
            server = new ServerSocket(getNewPort(), TCP_CONNECTION_BACKLOG, address);
            Log.d(TAG, "Data socket pasv() listen successful");
            return server.getLocalPort();
        } catch (Exception e) {
            //
        }
        return 0;
    }

    public int onEpsv(InetAddress address, boolean ssl) {
        clearState();
        if (ssl) {
            try {
                // Listen on any port (port parameter 0)
                sslServer = ftpsSockets.createSSLServerSocketEpsv(address);
                Log.d(TAG, "Data socket epsv() listen successful");
                return sslServer.getLocalPort();
            } catch (Exception e) {
                Log.e(TAG, "Data socket creation error");
                logging.appendLog("Data socket creation ex: " + e.getMessage());
                clearState();
                return 0;
            }
        }
        return onEpsvPlain(address);
    }

    public static int getNewPort() {
        int newPort = 0;
        final int portRangeLow = FsSettings.getPortRangeLow();
        final int portRangeHigh = FsSettings.getPortRangeHigh();
        if (portRangeLow != 0 || portRangeHigh != 0) {
            int i = new Random().nextInt(portRangeHigh - portRangeLow);
            newPort = portRangeLow + i;
        }
        return newPort;
    }

    public boolean onPort(InetAddress remoteAddress, int remotePort) {
        clearState();
        this.remoteAddress = remoteAddress;
        this.remotePort = remotePort;
        return true;
    }

    public void onEprt(Inet6Address remoteAddress, int remotePort) {
        clearState();
        this.remote6Address = remoteAddress;
        this.remote6Port = remotePort;
    }

    public Socket onTransfer() {
        return plain();
    }

    public SSLSocket onTransferSSL() {
        return ssl();
    }

    private SSLSocket ssl() {
        if (sslServer == null) {
            // We're in PORT mode (not PASV)
            if ((remoteAddress == null || remotePort == 0) && (remote6Address == null || remote6Port == 0)) {
                Log.i(TAG, "PORT mode but not initialized correctly");
                clearState();
                return null;
            }
            SSLSocket socket;
            try {
                if (remote6Address != null) socket = ftpsSockets.createSSLSocket6(remote6Address, remote6Port);
                else socket = ftpsSockets.createSSLSocket(remoteAddress, remotePort);
            } catch (Exception e) {
                if (remote6Address != null) {
                    Log.i(TAG, "Couldn't open PORT data socket to: " + remote6Address.toString()
                            + ":" + remote6Port);
                } else {
                    Log.i(TAG, "Couldn't open PORT data socket to: " + remoteAddress.toString()
                            + ":" + remotePort);
                }
                clearState();
                return null;
            }
            socket.addHandshakeCompletedListener(event -> {
                logging.appendLog("Handshake completed");
                try {
                    event.getSocket().setSoTimeout(0);
                } catch (SocketException e) {
                    throw new RuntimeException(e);
                }
            });
            logging.appendLog("Begin FTPS handshake");
            try {
                socket.setSoTimeout(30000);
                socket.startHandshake();
            } catch (IOException e) {
                return null;
            }
            return socket;
        } else {
            // We're in PASV mode (not PORT)
            final SSLSocket socket;
            try {
                sslServer.setSoTimeout(30000);
                socket = (SSLSocket) sslServer.accept();
                sslServer.setSoTimeout(0);
                socket.setTcpNoDelay(true);
                changeSocketTimeout(socket, 30000); // require this before handshake (see catch block)
                socket.addHandshakeCompletedListener(event -> {
                    logging.appendLog("Handshake completed");
                    changeSocketTimeout(socket, 0);
                });
                logging.appendLog("Begin FTPS handshake");
                socket.startHandshake();
            } catch (Exception e) {
                // Confirmed that some clients will timeout on first use of data connection so do
                // a handshake and find out right here and now.
                if (remoteAddress != null) {
                    Log.i(TAG, "Couldn't open PORT data socket to: " + remoteAddress.toString()
                            + ":" + remotePort);
                }
                clearState();
                return null;
            }
            clearState();
            return socket; // will be null if error occurred
        }
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

    private Socket plain() {
        if (server == null) {
            // We're in PORT mode (not PASV)
            if (remoteAddress == null || remotePort == 0) {
                Log.i(TAG, "PORT mode but not initialized correctly");
                clearState();
                return null;
            }
            Socket socket;
            try {
                socket = new Socket(remoteAddress, remotePort);
            } catch (IOException e) {
                Log.i(TAG, "Couldn't open PORT data socket to: " + remoteAddress.toString()
                        + ":" + remotePort);
                clearState();
                return null;
            }

            // Kill the socket if nothing happens for X milliseconds
            try {
                socket.setSoTimeout(SO_TIMEOUT_MS);
            } catch (Exception e) {
                Log.e(TAG, "Couldn't set SO_TIMEOUT");
                clearState();
                return null;
            }

            return socket;
        } else {
            // We're in PASV mode (not PORT)
            Socket socket = null;
            try {
                server.setSoTimeout(30000);
                socket = server.accept();
                server.setSoTimeout(0);
                Log.d(TAG, "onTransfer pasv accept successful");
            } catch (Exception e) {
                Log.i(TAG, "Exception accepting PASV socket: " + Arrays.toString(e.getStackTrace()));
                socket = null;
            }
            clearState();
            return socket; // will be null if error occurred
        }
    }

    /**
     * Return the port number that the remote client should be informed of (in the body of
     * the PASV response).
     *
     * @return The port number, or -1 if error.
     */
    public int getPortNumber() {
        if (server != null) {
            return server.getLocalPort(); // returns -1 if server socket is unbound
        } else {
            return -1;
        }
    }

    public InetAddress getPasvIp() {
        return FsService.getLocalInetAddress();
    }

    public void reportTraffic(long bytes) {
        // ignore, we don't care about how much traffic goes over wifi.
    }
}
