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
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.Arrays;
import java.util.Random;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLSocket;

import be.ppareit.swiftp.FsService;
import be.ppareit.swiftp.utils.FTPSSockets;
import be.ppareit.swiftp.utils.Logging;

public class LocalDataSocket {
    private static final String TAG = LocalDataSocket.class.getSimpleName();

    // How long the data connection is given to come up: the accept in passive mode, the connect
    // in active mode, and the TLS handshake on either. Not a transfer timeout, the socket goes
    // back to blocking forever once it is up, or a slow client loses its transfer halfway. The
    // client asked for this transfer, so it connects within a round trip or something in between
    // is dropping it, and waiting longer only freezes the client for longer.
    // Field, not a constant, to ease setting it in tests.
    int setupTimeoutMs = 10000;
    public static final int TCP_CONNECTION_BACKLOG = 5;

    // Bounds for passive port range. Below 1024 we would need extra privileges
    private static final int FIRST_UNPRIVILEGED_PORT = 1024;
    private static final int LAST_PORT = 65535;

    // Listener socket used for PASV mode
    ServerSocket server = null;
    SSLServerSocket sslServer = null;
    // Remote IP & port information used for PORT mode
    private InetAddress remoteAddress = null;
    private int remotePort;
    private Inet6Address remote6Address = null;
    private int remote6Port;
    private final Settings settings;
    private final Logging logging;

    private final FTPSSockets ftpsSockets = new FTPSSockets();

    // Why the last transfer attempt failed to get a data socket, phrased as a single
    // line fit to be sent as the text of a 425 reply. Null while nothing went wrong.
    private volatile String failureReason = null;

    public LocalDataSocket(Settings settings) {
        this.settings = settings;
        this.logging = new Logging(settings.isLoggingEnabled());
        clearState();
    }

    /**
     * Clears the state of this object, as if no pasv() or port() had occurred. All
     * sockets are closed.
     *
     * A transfer clears the state itself, so the only caller that matters from
     * outside is the end of the session: a client that asks for a passive port and
     * then leaves without using it would otherwise keep that port bound for as long
     * as the process lives.
     */
    public void clearState() {
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
                sslServer = ftpsSockets.createSSLServerSocket(getNewPort(settings));
                Log.d(TAG, "Data socket pasv() listen successful");
                return sslServer.getLocalPort();
            }
            server = new ServerSocket(getNewPort(settings), TCP_CONNECTION_BACKLOG);
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
            server = new ServerSocket(getNewPort(settings), TCP_CONNECTION_BACKLOG, address);
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
                sslServer = ftpsSockets.createSSLServerSocketEpsv(address, getNewPort(settings));
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

    /**
     * Picks the local port to listen on for a passive mode data connection.
     *
     * A field left at 0 means "not configured":, which asks the system for any
     * free port. Only the low field set means there is no upper bound, only the
     * high field set means no lower bound and low equal to high pins that single
     * port.
     *
     * Static, and told its settings, because the socket factory in FTPSSockets needs a port
     * too and has no session to take one from.
     *
     * @return the port to bind, or 0 to let the system choose.
     */
    public static int getNewPort(Settings settings) {
        int low = settings.getPortRangeLow();
        int high = settings.getPortRangeHigh();
        if (low <= 0 && high <= 0) {
            return 0;
        }
        if (low <= 0) low = FIRST_UNPRIVILEGED_PORT;
        if (high <= 0) high = LAST_PORT;
        if (low > LAST_PORT) low = LAST_PORT;
        if (high > LAST_PORT) high = LAST_PORT;
        if (low > high) { // hmmm, wrong order
            int swap = low;
            low = high;
            high = swap;
        }
        // low == high would otherwise give 0 and nextInt(0) throws
        return low + new Random().nextInt(high - low + 1);
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
        failureReason = null;
        return plain();
    }

    public SSLSocket onTransferSSL() {
        failureReason = null;
        return ssl();
    }

    /**
     * @return why the last onTransfer()/onTransferSSL() returned null, or null if the
     * last attempt succeeded or none has been made.
     */
    public String getFailureReason() {
        return failureReason;
    }

    private String noConnectionMessage(int port) {
        return "No data connection on port " + port + " after " + (setupTimeoutMs / 1000) + "s";
    }

    private SSLSocket ssl() {
        if (sslServer == null) {
            // We're in PORT mode (not PASV)
            if ((remoteAddress == null || remotePort == 0) && (remote6Address == null || remote6Port == 0)) {
                failureReason = "No data connection set up, send PASV or PORT first";
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
                    failureReason = "Could not connect data socket to "
                            + remote6Address.getHostAddress() + " port " + remote6Port;
                } else {
                    failureReason = "Could not connect data socket to "
                            + remoteAddress.getHostAddress() + " port " + remotePort;
                }
                Log.i(TAG, "Couldn't open PORT data socket: " + failureReason);
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
                socket.setSoTimeout(setupTimeoutMs);
                socket.startHandshake();
            } catch (IOException e) {
                failureReason = "TLS handshake failed on data connection to "
                        + socket.getInetAddress().getHostAddress() + " port " + socket.getPort();
                return null;
            }
            return socket;
        } else {
            // We're in PASV mode (not PORT)
            final SSLSocket socket;
            final int port = sslServer.getLocalPort();
            // Accepting and handshaking are separate tries: a timeout on the accept
            // means nobody arrived, a failure after it means somebody did and the TLS
            // negotiation went wrong. The 425 says which.
            try {
                sslServer.setSoTimeout(setupTimeoutMs);
                socket = (SSLSocket) sslServer.accept();
                sslServer.setSoTimeout(0);
            } catch (SocketTimeoutException e) {
                failureReason = noConnectionMessage(port);
                Log.i(TAG, failureReason);
                clearState();
                return null;
            } catch (Exception e) {
                failureReason = "Error opening data socket on port " + port;
                Log.i(TAG, failureReason + ": " + e.getMessage());
                clearState();
                return null;
            }
            try {
                socket.setTcpNoDelay(true);
                changeSocketTimeout(socket, setupTimeoutMs); // require this before handshake (see catch block)
                socket.addHandshakeCompletedListener(event -> {
                    logging.appendLog("Handshake completed");
                    changeSocketTimeout(socket, 0);
                });
                logging.appendLog("Begin FTPS handshake");
                // Confirmed that some clients will timeout on first use of data connection so do
                // a handshake and find out right here and now.
                socket.startHandshake();
            } catch (Exception e) {
                failureReason = "TLS handshake failed on data connection on port " + port;
                Log.i(TAG, failureReason + ": " + e.getMessage());
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
                failureReason = "No data connection set up, send PASV or PORT first";
                Log.i(TAG, "PORT mode but not initialized correctly");
                clearState();
                return null;
            }
            Socket socket = new Socket();
            try {
                // With a timeout: an unanswered connect to a filtered port otherwise sits on the
                // OS default, which is minutes.
                socket.connect(new InetSocketAddress(remoteAddress, remotePort), setupTimeoutMs);
            } catch (IOException e) {
                failureReason = "Could not connect data socket to "
                        + remoteAddress.getHostAddress() + " port " + remotePort;
                Log.i(TAG, "Couldn't open PORT data socket: " + failureReason);
                clearState();
                return null;
            }

            // No read timeout on the socket itself, like the passive and TLS paths, which both
            // put theirs back to 0 once the connection is up.
            return socket;
        } else {
            // We're in PASV mode (not PORT)
            Socket socket = null;
            final int port = server.getLocalPort();
            try {
                server.setSoTimeout(setupTimeoutMs);
                socket = server.accept();
                server.setSoTimeout(0);
                Log.d(TAG, "onTransfer pasv accept successful");
            } catch (SocketTimeoutException e) {
                // The client asked for this transfer, so it has not gone away: something
                // between it and this port is dropping the connection.
                failureReason = noConnectionMessage(port);
                Log.i(TAG, failureReason);
                socket = null;
            } catch (Exception e) {
                failureReason = "Error opening data socket on port " + port;
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
