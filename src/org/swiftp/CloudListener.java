package org.swiftp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.util.Log;

public class CloudListener extends Thread {
	public static final int IN_BUF_SIZE = 2048;
	public static final String ENCODING = "UTF-8";
	private FTPServerService ftpServerService;
	MyLog myLog = new MyLog(getClass().getName());
	private byte[] outboundData = null;
	private byte[] inboundData = null;
	
	/* We establish a so-called "command session" to the proxy. New connections
	 * will be handled by creating addition control and data connections to the
	 * proxy. See proxy_protocol.txt and proxy_architecture.pdf for an
	 * explanation of how proxying works. 
	 */ 
	
	public CloudListener(FTPServerService ftpServerService) {
		this.ftpServerService = ftpServerService;
	}
	
	public void run() {
		Socket commandSocket;
		String[] candidateProxies = getProxyList();
		for(String hostname : candidateProxies) {
			try {
				commandSocket = new Socket(hostname, Defaults.cloudProxyPort);
			} catch(IOException e) {
				myLog.l(Log.INFO, "Failed proxy connection to: " + hostname + ",  trying next");
				continue;
			}
			myLog.l(Log.INFO, "Proxy connection open, authenticating");
			JSONObject json = new JSONObject();
			String secret = retrieveSecret();
			try {
				json.put("android_id", Util.getAndroidId());
				json.put("swiftp_version", Util.getVersion());
				if(secret == null) {
					// We don't have a secret stored. This implies that we haven't yet
					// created an account, or that our account information was somehow
					// lost. In either case, we need to send a create_account request
					// to the proxy. The fields are "android_id" and "action."
					json.put("action", "create_account");
				} else {
					// We have a valid secret, so do an "authenticate" request
					json.put("action", "authenticate");
					json.put("secret", secret);
				}
				try {
					OutputStream out = commandSocket.getOutputStream();
					InputStream in = commandSocket.getInputStream();
					int numBytes;
					
					out.write(json.toString().getBytes(ENCODING));
					// Read and parse the server's response
					byte[] bytes = new byte[IN_BUF_SIZE];
					// Here we assume that the server's response will all be contained in
					// a single read, which may be unsafe for large responses
					numBytes = in.read(bytes);
					json = new JSONObject(new String(bytes, 0, numBytes, ENCODING));
					if(checkAndPrintJsonError(json)) {
						return;
					}
					
					// If we reach here, we have a response to our create_account or authenticate
					// request that indicates success.
					if(secret == null) {
						// If our local variable "secret" is null, then we did a create_account request,
						// and the proxy should have replied with a object containing a field named
						// "secret" that will be our persistent secret.
						if(!json.has("secret")) {
							myLog.l(Log.INFO, "Proxy didn't reply to create_account with secret");
							return;
						}
						secret = json.getString("secret");
						storeSecret(secret);
					}
					
					// Now that we have authenticated, we want to start the command session so we can
					// be notified of pending control sessions.
					json = new JSONObject();
					json.put("action", "start_command_session");
					out.write(json.toString().getBytes(ENCODING));
					numBytes = in.read(bytes);
					
					// Get the response to the start_command_session request
					json = new JSONObject(new String(bytes, 0, numBytes, ENCODING));
					if(checkAndPrintJsonError(json)) {
						return;
					}
					if(!json.has("prefix")) {
						myLog.l(Log.INFO, "start_command_session didn't receive a prefix in response");
						return;
					}
					String prefix = json.getString("prefix");
					while(true) {
						numBytes = in.read(bytes);
						if(numBytes == -1) {
							continue;
						}
						if(outboundData != null) {
							myLog.l(Log.DEBUG, "Outbound command data queued, sending");
							out.write(outboundData);
						}
						// Get the response to the request that we just sent
						numBytes = in.read(bytes);
						inboundData = new byte[numBytes];
						System.arraycopy(bytes, 0, inboundData, 0, numBytes);
					}
					// Think about: how to prevent multiple simultaneous requests
					//              how the datasocketfactory finds the valid cloudlistener
					//              how to multiplex between listening and sending requests
					//              how to prevent simultaneous requests by both parties
					
				} catch (IOException e) {
					myLog.l(Log.INFO, "IOException in command session: " + e);
					return;
				}
			} catch (JSONException e) {
				myLog.l(Log.ERROR, "JSONException: " + e);
				return;
			}
			
			
		}
	}
	
	private String[] getProxyList() {
		// TODO: retrieve this from the net instead of hardcoding
		return new String[] {"c1.swiftp.org"};
	}
	
	private boolean checkAndPrintJsonError(JSONObject json) throws JSONException {
		if(json.has("errorCode")) {
			// The returned JSON object will have a field called "errorCode"
			// if there was a problem executing our request.
			StringBuilder s = new StringBuilder(
					"Error response connecting command socket, code: ");
			s.append(json.getString("errorCode"));
			if(json.has("errorString")) {
				s.append(", string: ");
				s.append(json.getString("errorString"));
			}
			myLog.l(Log.INFO, s.toString());
			return true;
		}
		return false;
	}
	
	/**
	 * Reads our persistent storage, looking for a stored proxy authentication secret.
	 * @return The secret, if present, or null.
	 */
	private String retrieveSecret() {
		SharedPreferences settings = Globals.getContext().
			getSharedPreferences(Defaults.getSettingsName(),
			Defaults.getSettingsMode());
		return settings.getString("proxySecret", null);
	}
	
	private void storeSecret(String secret) {
		SharedPreferences settings = Globals.getContext().
			getSharedPreferences(Defaults.getSettingsName(),
			Defaults.getSettingsMode());
		Editor editor = settings.edit();
		editor.putString("proxySecret", secret);
		editor.commit();
	}
	
}

