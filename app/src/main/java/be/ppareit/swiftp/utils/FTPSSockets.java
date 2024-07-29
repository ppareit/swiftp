package be.ppareit.swiftp.utils;

import android.content.SharedPreferences;
import android.os.Build;
import android.util.Log;

import androidx.security.crypto.EncryptedSharedPreferences;
import androidx.security.crypto.MasterKey;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.Socket;
import java.security.GeneralSecurityException;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.UnrecoverableKeyException;
import java.util.Set;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;

import be.ppareit.swiftp.App;
import be.ppareit.swiftp.FsSettings;
import be.ppareit.swiftp.server.LocalDataSocket;

public class FTPSSockets {

    private static final String TAG = FTPSSockets.class.getSimpleName();
    private final Logging logging = new Logging();

    private static final String KEYSTORE_CERT_FILENAME = "storej.jks";
    private static final String TRUST_STORE_CERT_FILENAME = "storeb.bks";

    // Need to have static to deal with TLS resumption and consume less time for performance increase.
    // Allows it to maintain the assigned values on new FTPSSockets() use.
    // Use synchronized when working with to maintain thread safety.
    private static SSLServerSocketFactory serverSocketFactory = null;
    private static SSLSocketFactory socketFactory = null;
    private static SSLContext context = null;

    public FTPSSockets() {
    }

    /*
     * Gets the server socket factory from the static holder.
     * */
    public static synchronized SSLServerSocketFactory getServerSocketFactory() {
        return serverSocketFactory;
    }

    /*
     * Gets the socket factory from the static holder.
     * */
    public static synchronized SSLSocketFactory getSocketFactory() {
        return socketFactory;
    }

    /*
     * Puts the socket factory to static holder.
     * */
    public static synchronized void putSocketFactory(SSLSocketFactory factory) {
        socketFactory = factory;
    }

    /*
     * Puts the server socket factory to static holder.
     * */
    public static synchronized void putServerSocketFactory(SSLServerSocketFactory factory) {
        serverSocketFactory = factory;
    }

    /*
     * Gets the static context.
     * */
    public static synchronized SSLContext getContext() {
        return context;
    }

    /*
     * Puts the context to static holder.
     * */
    public static synchronized void putContext(SSLContext context) {
        FTPSSockets.context = context;
    }

    /*
     * Obtains a new server socket as needed.
     * */
    public SSLServerSocket initServerSocket() throws IOException {
        setContext();
        setServerSocketFactory();
        putSocketFactory(null); // Set null to not block on certificate changes.
        return createServerSocket();
    }

    /*
     * Sets up the context.
     * */
    private void setContext() {
        try {
            KeyManagerFactory kmFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
            KeyStore ks = getKeyStore(KEYSTORE_CERT_FILENAME);
            if (ks == null) return;
            kmFactory.init(ks, getCertPass());
            TrustManager[] tm = getTrustManager();
            if (tm == null) return;
            putContext(SSLContext.getInstance("TLS"));
            getContext().init(kmFactory.getKeyManagers(), tm, new SecureRandom());
        } catch (Exception e) {
            //
        }
    }

    /*
     * Gets the factory from the content.
     * */
    private void setServerSocketFactory() {
        putServerSocketFactory(getContext().getServerSocketFactory());
    }

    /*
     * Re-obtains a factory as needed.
     * */
    private SSLSocketFactory getNewSocketFactory(String type) throws KeyStoreException,
            NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        KeyManagerFactory kmFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        // Need to try to maintain resumption with data sockets in conflict with implicit
        if (getContext() != null) return getContext().getSocketFactory();
        // When implicit port is disabled, create it here.
        kmFactory.init(getKeyStore(KEYSTORE_CERT_FILENAME), getCertPass());
        SSLContext context = SSLContext.getInstance(type);
        context.init(kmFactory.getKeyManagers(), getTrustManager(), new SecureRandom());
        putContext(context);
        putServerSocketFactory(context.getServerSocketFactory());
        return context.getSocketFactory();
    }

    public static char[] getCertPass() {
        try {
            MasterKey masterKey = new MasterKey.Builder(App.getAppContext())
                    .setKeyScheme(MasterKey.KeyScheme.AES256_GCM)
                    .build();

            SharedPreferences encryptedSharedPreferences = EncryptedSharedPreferences.create(
                    App.getAppContext(),
                    "encrypted_prefs",
                    masterKey,
                    EncryptedSharedPreferences.PrefKeyEncryptionScheme.AES256_SIV,
                    EncryptedSharedPreferences.PrefValueEncryptionScheme.AES256_GCM
            );

            return encryptedSharedPreferences.getString("certPass", "").toCharArray();

        } catch (GeneralSecurityException | IOException e) {
            //
        }
        return new char[0];
    }

    /*
     * Saves the certificate password internally.
     * */
    public static void putCertPass(String s) {
        try {
            MasterKey masterKey = new MasterKey.Builder(App.getAppContext())
                    .setKeyScheme(MasterKey.KeyScheme.AES256_GCM)
                    .build();

            SharedPreferences encryptedSharedPreferences = null;
            encryptedSharedPreferences = EncryptedSharedPreferences.create(
                    App.getAppContext(),
                    "encrypted_prefs",
                    masterKey,
                    EncryptedSharedPreferences.PrefKeyEncryptionScheme.AES256_SIV,
                    EncryptedSharedPreferences.PrefValueEncryptionScheme.AES256_GCM
            );

            SharedPreferences.Editor editor = encryptedSharedPreferences.edit();

            editor.putString("certPass", s).apply();

        } catch (GeneralSecurityException | IOException e) {
            //
        }
    }

    /*
     * Keystore required here for implicit connection use
     * Load the keystore cert .jks file using getInstance() of BKS as JKS isn't supported.
     * */
    private static KeyStore getKeyStore(String filename) {
        try {
            String s = KeyStore.getDefaultType(); // Will be BKS (Bouncy Castle)
            KeyStore keyStore = KeyStore.getInstance(s);
            //KeyStore keyStore = KeyStore.getInstance("PKCS12"); // Also works here using p12 file
            FileInputStream fis = getCertInputStream(filename);
            if (fis == null) return null;
            keyStore.load(fis, getCertPass());
            fis.close();
            return keyStore;
        } catch (Exception e) {
            return null;
        }
    }

    /*
     * Reads the internal certificate file.
     * */
    private static FileInputStream getCertInputStream(String filename) {
        try {
            File cacheFile = new File(App.getAppContext().getCacheDir(), filename);
            long fileSize = cacheFile.length();
            if (fileSize == 0) return null;
            return new FileInputStream(cacheFile);
        } catch (Exception e) {
            return null;
        }
    }

    /*
     * Load the trust store cert .bks file using getInstance() of BKS.
     * */
    private static TrustManager[] getTrustManager() {
        try {
            String s = TrustManagerFactory.getDefaultAlgorithm();
            TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance(s);
            trustManagerFactory.init(getKeyStore(TRUST_STORE_CERT_FILENAME));
            return trustManagerFactory.getTrustManagers();
        } catch (Exception e) {
            return null; // Can be null (not absolutely required). Will use default trust instead.
        }
    }

    /*
     * Checks the internal keystore file to see if its working.
     * Will return false if the cert is broken, no password, or wrong password.
     * */
    public static boolean checkKeyStore() {
        return getKeyStore(KEYSTORE_CERT_FILENAME) != null;
    }

    /*
     * Checks the internal trust store file to see if its working.
     * Will return false if the cert is broken, no password, or wrong password.
     * */
    public static boolean checkTrustStore() {
        return getKeyStore(TRUST_STORE_CERT_FILENAME) != null;
    }

    /*
     * Deletes the internal keystore file.
     * */
    public static void deleteKeyStore() {
        File cacheFile = new File(App.getAppContext().getCacheDir(), KEYSTORE_CERT_FILENAME);
        cacheFile.delete();
    }

    /*
     * Deletes the internal trust store file.
     * */
    public static void deleteTrustStore() {
        File cacheFile = new File(App.getAppContext().getCacheDir(), TRUST_STORE_CERT_FILENAME);
        cacheFile.delete();
    }

    /*
     * Creates the implicit socket.
     * */
    private SSLServerSocket createServerSocket() throws IOException {
        SSLServerSocket sslServerSocket = (SSLServerSocket) getServerSocketFactory()
                .createServerSocket(FsSettings.getImplicitPort());
        sslServerSocket.setReuseAddress(false);
        sslServerSocket.setReceiveBufferSize(4);
        sslServerSocket.setEnableSessionCreation(true); // true is default; false breaks things
        sslServerSocket.setEnabledProtocols(getChosenProtocols(sslServerSocket.getSupportedProtocols()));
        sslServerSocket.setEnabledCipherSuites(sslServerSocket.getSupportedCipherSuites());
        //sslServerSocket.setNeedClientAuth(true); // officially buggy; doesn't work (use SSLParameters)
        //sslServerSocket.setWantClientAuth(false); // officially buggy; doesn't work (use SSLParameters)
        if (Build.VERSION.SDK_INT >= 24) sslServerSocket.setSSLParameters(getClientCertParams());
        sslServerSocket.setUseClientMode(false); // Of no use here; true only breaks things
        return sslServerSocket;
    }

    /*
     * Creates the socket for AUTH.
     * */
    public SSLSocket createAuthSocket(String type, Socket socket) {
        SSLSocket sslSocket;
        try {
            if (getSocketFactory() == null) putSocketFactory(getNewSocketFactory(type));
            sslSocket = (SSLSocket) getSocketFactory().createSocket(socket, null, socket.getPort(), false);
            sslSocket.setReuseAddress(true);
            sslSocket.setEnabledProtocols(getChosenProtocols(sslSocket.getSupportedProtocols()));
            sslSocket.setEnabledCipherSuites(sslSocket.getSupportedCipherSuites());
            if (Build.VERSION.SDK_INT >= 24) sslSocket.setSSLParameters(getClientCertParams());
            sslSocket.setUseClientMode(false);
            //sslSocket.setReceiveBufferSize();
            sslSocket.setSoTimeout(30000);
            sslSocket.addHandshakeCompletedListener(event -> {
                Log.i(TAG, "FTPS handshake completed");
                logging.appendLog("Handshake completed");
                try {
                    event.getSocket().setSoTimeout(0);
                } catch (Exception e) {
                    //
                }
            });
            Log.i(TAG, "Begin FTPS handshake");
            logging.appendLog("Begin FTPS handshake");
            sslSocket.startHandshake();
        } catch (Exception e) {
            Log.e(TAG, type + " failed: " + e.getLocalizedMessage());
            sslSocket = null;
        }
        return sslSocket;
    }

    public SSLServerSocket createSSLServerSocketEpsv(InetAddress address) throws IOException {
        SSLServerSocket sslServerSocket;
        sslServerSocket = (SSLServerSocket) FTPSSockets.getServerSocketFactory().createServerSocket(
                LocalDataSocket.getNewPort(),
                LocalDataSocket.TCP_CONNECTION_BACKLOG,
                address
        );
        sslServerSocket.setReuseAddress(true);
        sslServerSocket.setEnabledProtocols(getChosenProtocols(sslServerSocket.getSupportedProtocols()));
        sslServerSocket.setEnabledCipherSuites(sslServerSocket.getSupportedCipherSuites()); //ignored Android 10+
        if (Build.VERSION.SDK_INT >= 24) sslServerSocket.setSSLParameters(getClientCertParams());
        sslServerSocket.setUseClientMode(false);
        sslServerSocket.setEnableSessionCreation(true);
        return sslServerSocket;
    }

    public SSLServerSocket createSSLServerSocket() throws IOException {
        SSLServerSocket sslServerSocket;
        //SSLSocketFactory factory = SSLContext.getDefault().getSocketFactory();
        sslServerSocket = (SSLServerSocket) FTPSSockets.getServerSocketFactory().createServerSocket(
                LocalDataSocket.getNewPort(),
                LocalDataSocket.TCP_CONNECTION_BACKLOG
        );
        sslServerSocket.setReuseAddress(true);
        sslServerSocket.setEnabledProtocols(getChosenProtocols(sslServerSocket.getSupportedProtocols()));
        sslServerSocket.setEnabledCipherSuites(sslServerSocket.getSupportedCipherSuites()); //ignored Android 10+
        if (Build.VERSION.SDK_INT >= 24) sslServerSocket.setSSLParameters(getClientCertParams());
        sslServerSocket.setUseClientMode(false); // Clients don't like true here; true only breaks things
        sslServerSocket.setEnableSessionCreation(true); // true is default; false breaks things
        return sslServerSocket;
    }

    public SSLSocket createSSLSocket(InetAddress remoteAddress, int remotePort) throws IOException {
        Socket plainSocket = new Socket(remoteAddress, remotePort);
        //SSLSocketFactory factory = SSLContext.getDefault().getSocketFactory();
        SSLSocketFactory factory = FTPSSockets.getSocketFactory();
        SSLSocket sslSocket = (SSLSocket) factory.createSocket(plainSocket,
                remoteAddress.getHostAddress(),
                remotePort,
                false
        );
        sslSocket.setReuseAddress(true);
        sslSocket.setEnabledProtocols(getChosenProtocols(sslSocket.getSupportedProtocols()));
        sslSocket.setEnabledCipherSuites(sslSocket.getSupportedCipherSuites()); //ignored Android 10+
        if (Build.VERSION.SDK_INT >= 24) sslSocket.setSSLParameters(getClientCertParams());
        sslSocket.setUseClientMode(false);
        sslSocket.setEnableSessionCreation(true);
        return sslSocket;
    }

    public SSLSocket createSSLSocket6(Inet6Address remote6Address, int remote6Port) throws IOException {
        Socket plainSocket = new Socket(remote6Address, remote6Port);
        //SSLSocketFactory factory = SSLContext.getDefault().getSocketFactory();
        SSLSocketFactory factory = FTPSSockets.getSocketFactory();
        SSLSocket sslSocket = (SSLSocket) factory.createSocket(
                plainSocket,
                remote6Address.getHostAddress(),
                remote6Port,
                false
        );
        sslSocket.setReuseAddress(true);
        sslSocket.setEnabledProtocols(getChosenProtocols(sslSocket.getSupportedProtocols()));
        sslSocket.setEnabledCipherSuites(sslSocket.getSupportedCipherSuites()); //ignored Android 10+
        if (Build.VERSION.SDK_INT >= 24) sslSocket.setSSLParameters(getClientCertParams());
        sslSocket.setUseClientMode(false);
        sslSocket.setEnableSessionCreation(true);
        return sslSocket;
    }

    /*
     * Enables use of requiring client certificate for connecting. This is used to improve security.
     * When used, all clients without the pkcs12 cert in use will be denied.
     * */
    private SSLParameters getClientCertParams() {
        SSLParameters params = new SSLParameters();
        // Java is broken with the direct socket setting of this so bye bye Android 6.
        if (FsSettings.useClientCert()) {
            params.setNeedClientAuth(true);
            return params;
        }
        return params;
    }

    /*
    * Gets the protocols as chosen by the user or returns the default list if nothing has been chosen.
    * This is used to improve connection security. If for example, TLSv1.1 is not in the list, then
    * a client cannot try to partially downgrade to TLSv1.1 as it will be denied.
    * */
    private String[] getChosenProtocols(String[] socketProtocols) {
        String[] s = socketProtocols;
        final Set<String> list = FsSettings.getAllowedProtocols();
        if (!list.isEmpty()) {
            s = new String[list.size()];
            list.toArray(s);
        }
        return s;
    }

    /*
    * Returns a dynamic list of supported protocols for the socket/device.
    * Examples:
    * Android 8: TLSv1, TLSv1.1, TLSv1.2
    * Android 14: TLSv1, TLSv1.1, TLSv1.2, TLSv1.3
    * */
    public static CharSequence[] getSupportedProtocols() {
        try {
            SSLServerSocket socket = (SSLServerSocket) SSLServerSocketFactory.getDefault().createServerSocket();
            CharSequence[] list = socket.getSupportedProtocols();
            socket.close();
            return list;
        } catch (Exception e) {
            return new CharSequence[0];
        }
    }
}