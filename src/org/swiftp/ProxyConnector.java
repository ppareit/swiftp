package org.swiftp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.LinkedList;
import java.util.Queue;

import org.json.JSONException;
import org.json.JSONObject;

import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.util.Log;

public class ProxyConnector extends Thread {
	public static final int IN_BUF_SIZE = 2048;
	public static final String ENCODING = "UTF-8";
	private FTPServerService ftpServerService;
	MyLog myLog = new MyLog(getClass().getName());
	JSONObject response = null;
	Thread responseWaiter = null;
	Queue<Thread> queuedRequestThreads = new LinkedList<Thread>();
	Socket commandSocket = null;
	OutputStream out = null;
	public static final int RESPONSE_WAIT_MS = 10000;
	public static final int QUEUE_WAIT_MS = 20000;
	String hostname = null;
	InputStream inputStream = null;
	QuotaStats cachedQuotaStats = null;
	
	/* We establish a so-called "command session" to the proxy. New connections
	 * will be handled by creating addition control and data connections to the
	 * proxy. See proxy_protocol.txt and proxy_architecture.pdf for an
	 * explanation of how proxying works. Hint: it's complicated.
	 */ 
	
	public ProxyConnector(FTPServerService ftpServerService) {
		this.ftpServerService = ftpServerService;
	}
	
	public void run() {
		try {
			String candidateProxies[] = getProxyList();
			for(String hostname : candidateProxies) {
				commandSocket = newAuthedSocket(hostname, Defaults.REMOTE_PROXY_PORT);
				if(commandSocket != null) {
					this.hostname = hostname;
					break;
				}
			}
			if(commandSocket == null) {
				myLog.l(Log.INFO, "No proxies accepted connection, failing.");
				return;
			}
			// Now that we have authenticated, we want to start the command session so we can
			// be notified of pending control sessions.
			JSONObject request = makeJsonRequest("start_command_session");
			response = sendRequest(commandSocket, request);
			if(response == null) {
				myLog.w("Couldn't create proxy command session");
				return;
			}
			if(!response.has("prefix")) {
				myLog.l(Log.INFO, "start_command_session didn't receive a prefix in response");
				return;
			}
			String prefix = response.getString("prefix");
			response = null;  // Indicate that response is free for other use
			myLog.l(Log.INFO, "Got prefix of: " + prefix + ", entering command loop");
			Globals.setProxyConnector(this);
			inputStream = commandSocket.getInputStream();
			out = commandSocket.getOutputStream();
			int numBytes;
			byte[] bytes = new byte[IN_BUF_SIZE];
			spawnQuotaRequester().start();
			while(true) {
				myLog.d("to proxy read()");
				numBytes = inputStream.read(bytes);
				myLog.d("from proxy read()");
				JSONObject incomingJson = null;
				if(numBytes > 0) {
					String responseString = new String(bytes, ENCODING);
					incomingJson = new JSONObject(responseString);
					if(incomingJson.has("action")) {
						// If the incoming JSON object has an "action" field, then it is a
						// request, and not a response
						incomingCommand(incomingJson);
					} else {
						// If the incoming JSON object does not have an "action" field, then
						// it is a response to a request we sent earlier.
						// If there's an object waiting for a response, then that object
						// will be referenced by responseWaiter.
						if(responseWaiter != null) {
							if(response != null) {
								myLog.l(Log.INFO, "Overwriting existing cmd session response");
							}
							response = incomingJson;
							responseWaiter.interrupt();
						} else {
							myLog.l(Log.INFO, "Response received but no responseWaiter");
						}
					}
				} else {
					myLog.l(Log.DEBUG, "Command socket short read, exiting");
					break;
				}
			}
			myLog.l(Log.INFO, "ProxyConnector thread quitting cleanly");
		} catch (IOException e) {
			myLog.l(Log.INFO, "IOException in command session: " + e);
			return;
		} catch (JSONException e) {
			myLog.l(Log.INFO, "Commmand socket JSONException: " + e);
			return;
		} finally {
			commandSocket = null;
			Globals.setProxyConnector(null);
			hostname = null;
		}
	}
	
	// This function is used to spawn a new Thread that will make a request over the
	// command thread. Since the main ProxyConnector thread handles the input
	// request/response de-multiplexing, it cannot also make a request using the
	// sendCmdSocketRequest, since sendCmdSocketRequest will block waiting for
	// a response, but the same thread is expected to deliver the response.
	// The short story is, if the main ProxyConnector command session thread wants to
	// make a request, the easiest way is to spawn a new thread and have it call
	// sendCmdSocketRequest in the same way as any other thread. 
	private Thread spawnQuotaRequester() {
		return new Thread() {
			public void run() {
				getQuotaStats(false);
			}
		};
	}
	
	private String[] getProxyList() {
		// TODO: retrieve this from the net instead of hardcoding, maybe store on s3/cloudfront
		if(Defaults.release) {
			myLog.l(Log.INFO, "getProxyList stub");
			return null;
		} else {
			return new String[] {"cdev.swiftp.org"};
		}
	}
	
	private boolean checkAndPrintJsonError(JSONObject json) throws JSONException {
		if(json.has("error_code")) {
			// The returned JSON object will have a field called "errorCode"
			// if there was a problem executing our request.
			StringBuilder s = new StringBuilder(
					"Error in JSON response, code: ");
			s.append(json.getString("error_code"));
			if(json.has("error_string")) {
				s.append(", string: ");
				s.append(json.getString("error_string"));
			}
			myLog.l(Log.INFO, s.toString());
			
			// Dev code to enable frequent database wipes. If we fail to login,
			// remove our stored account info, causing a create_account action
			// next time.
			if(!Defaults.release) {
				if(json.getInt("error_code") == 11) {
					myLog.l(Log.DEBUG, "Dev: removing secret due to login failure");
					removeSecret();
				}
			}
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
	
	private void removeSecret() {
		SharedPreferences settings = Globals.getContext().
			getSharedPreferences(Defaults.getSettingsName(),
					Defaults.getSettingsMode());
		Editor editor = settings.edit();
		editor.remove("proxySecret");
		editor.commit();
	}
	
	private void incomingCommand(JSONObject json) {
		try {
			String action = json.getString("action");
			if(action.equals("control_connection_waiting")) {
				startControlSession(json.getInt("port"));
			} else {
				myLog.l(Log.INFO, "Unsupported incoming action: " + action);
			}
			// If we're starting a control session register with ftpServerService
		} catch (JSONException e){
			myLog.l(Log.INFO, "JSONException in proxy incomingCommand");
		}
	}
	
	private void startControlSession(int port) {
		Socket socket;
		myLog.d("Starting new proxy FTP control session");
		socket = newAuthedSocket(hostname, port);
		if(socket == null) {
			myLog.w("startControlSession got null authed socket");
			return;
		}
		ProxyDataSocketFactory dataSocketFactory = new ProxyDataSocketFactory();
		SessionThread thread = new SessionThread(socket, dataSocketFactory, false);
		thread.start();
		ftpServerService.registerSessionThread(thread);
	}
	
	/**
	 * Connects an outgoing socket to the proxy and authenticates, creating an account
	 * if necessary.
	 */
	private Socket newAuthedSocket(String hostname, int port) {
		if(hostname == null) {
			myLog.w("newAuthedSocket can't connect to null host");
			return null;
		}
		JSONObject json = new JSONObject();
		String secret = retrieveSecret();
		Socket socket;
		OutputStream out = null;
		InputStream in = null;
		
		try {
			socket = new Socket(hostname, port);
			json.put("android_id", Util.getAndroidId());
			json.put("swiftp_version", Util.getVersion());
			if(secret == null) {
				// We don't have a secret stored. This implies that we haven't yet
				// created an account, or that our account information was somehow
				// lost. In either case, we need to send a create_account request
				// to the proxy. The fields are "android_id" and "action."
				myLog.l(Log.INFO, "Secret not present, attempting account creation");
				json.put("action", "create_account");
			} else {
				// We have a valid secret, so do an "authenticate" request
				myLog.l(Log.INFO, "Secret is present, attempting login");
				json.put("action", "authenticate");
				json.put("secret", secret);
			}
			out = socket.getOutputStream();
			in = socket.getInputStream();
			int numBytes;
			
			out.write(json.toString().getBytes(ENCODING));
			myLog.l(Log.DEBUG, "Sent authentication request");
			// Read and parse the server's response
			byte[] bytes = new byte[IN_BUF_SIZE];
			// Here we assume that the server's response will all be contained in
			// a single read, which may be unsafe for large responses
			numBytes = in.read(bytes);
			if(numBytes == -1) {
				myLog.l(Log.INFO, "Proxy socket closed while waiting for auth response");
				return null;
			} else if (numBytes == 0) {
				myLog.l(Log.INFO, "Short network read waiting for auth, quitting");
				return null;
			}
			json = new JSONObject(new String(bytes, 0, numBytes, ENCODING));
			if(checkAndPrintJsonError(json)) {
				return null;
			}
			
			// If we reach here, we have a response to our create_account or authenticate
			// request that indicates success.
			if(secret == null) {
				// If our local variable "secret" is null, then we did a create_account request,
				// and the proxy should have replied with a object containing a field named
				// "secret" that will be our persistent secret.
				if(!json.has("secret")) {
					myLog.l(Log.INFO, "Proxy didn't reply to create_account with secret");
					return null;
				}
				secret = json.getString("secret");
				storeSecret(secret);
			}
			myLog.d("newAuthedSocket successful");
			return socket;
		} catch(Exception e) {
			myLog.w("Exception during proxy connection or authentication: " + e);
			return null;
		}
	}
	
	public void quit() {
		try {
			sendRequest(commandSocket, makeJsonRequest("finished")); // ignore reply
			
			if(inputStream != null) {
				myLog.d("quit() closing proxy inputStream");
				inputStream.close();
			} else {
				myLog.d("quit() won't close null inputStream");
			}
			if(commandSocket != null) {
				myLog.d("quit() closing proxy socket");
				commandSocket.close();
			} else {
				myLog.d("quit() won't close null socket");
			}
		}  
		catch (IOException e) {} 
		catch(JSONException e) {}
		
	}
	
	private JSONObject sendCmdSocketRequest(JSONObject json) {
		try {
			boolean queued;
			synchronized(this) {
				if(responseWaiter == null) {
					responseWaiter = Thread.currentThread();
					queued = false;
					myLog.d("sendCmdSocketRequest proceeding without queue");
				} else if (!responseWaiter.isAlive()) {
					// This code should never run. It is meant to recover from a situation
					// where there is a thread that sent a proxy request but died before
					// starting the subsequent request. If this is the case, the correct
					// behavior is to run the next queued thread in the queue, or if the
					// queue is empty, to perform our own request. 
					myLog.l(Log.WARN, "Won't wait on dead responseWaiter");
					if(queuedRequestThreads.size() == 0) {
						responseWaiter = Thread.currentThread();
						queued = false;
					} else {
						queuedRequestThreads.add(Thread.currentThread());
						queuedRequestThreads.remove().interrupt(); // start queued thread
						queued = true;
					}
				} else {
					myLog.d("sendCmdSocketRequest queueing thread");
					queuedRequestThreads.add(Thread.currentThread());
					queued = true;
				}
			}
			// If a different thread has sent a request and is waiting for a response,
			// then the current thread will be in a queue waiting for an interrupt
			if(queued) {
				// The current thread must wait until we are popped off the waiting queue
				// and receive an interrupt()
				boolean interrupted = false;
				try {
					myLog.d("Queued cmd session request thread sleeping...");
					Thread.sleep(QUEUE_WAIT_MS);
				} catch (InterruptedException e) {
					myLog.l(Log.DEBUG, "Proxy request popped and ready");
					interrupted = true;
				}
				if(!interrupted) {
					myLog.l(Log.WARN, "Timed out waiting on proxy queue");
					return null;
				}
			}
			// We have been popped from the wait queue if necessary, and now it's time
			// to send the request.
			try {
				responseWaiter = Thread.currentThread();
				byte[] outboundData = Util.jsonToByteArray(json);
				try {
					out.write(outboundData);
				} catch(IOException e) {
					myLog.l(Log.WARN, "IOException sending proxy request");
					return null;
				}
				// Wait RESPONSE_WAIT_MS for a response from the proxy
				boolean interrupted = false;
				try {
					// Wait for the main ProxyConnector thread to interrupt us, meaning
					// that a response has been received.
					myLog.d("Cmd session request sleeping until response");
					Thread.sleep(RESPONSE_WAIT_MS);
				} catch (InterruptedException e) {
					myLog.d("Cmd session response received");
					interrupted = true;
				}
				if(!interrupted) {
					myLog.l(Log.WARN, "Proxy request timed out");
					return null;
				}
				// At this point, the main ProxyConnector thread will have stored
				// our response in "JSONObject response".
				myLog.d("Cmd session response was: " + response);
				return response;
			} 
			finally {
				// Make sure that when this request finishes, the next thread on the
				// queue gets started.
				synchronized(this) {
					if(queuedRequestThreads.size() != 0) {
						queuedRequestThreads.remove().interrupt();
					}
				}
			}
		} catch (JSONException e) {
			myLog.l(Log.INFO, "JSONException in sendRequest: " + e);
			return null;
		}
	}

	public JSONObject sendRequest(InputStream in, OutputStream out, JSONObject request) 
	throws JSONException {
		try {
			out.write(Util.jsonToByteArray(request));
			byte[] bytes = new byte[IN_BUF_SIZE];
			int numBytes = in.read(bytes);
			if(numBytes < 1) {
				myLog.w("Proxy sendRequest short read on reponse");
				return null;
			}
			JSONObject response = Util.byteArrayToJson(bytes);
			if(response == null) {
				myLog.w("Null response to sendRequest");
			}
			if(checkAndPrintJsonError(response)) {
				myLog.w("Error response to sendRequest");
				return null;
			}
			return response;
		} catch (IOException e) {
			myLog.w("IOException in proxy sendRequest: " + e);
			return null;
		}
	}
	
	public JSONObject sendRequest(Socket socket, JSONObject request) 
	throws JSONException {
		 try {
			 return sendRequest(socket.getInputStream(), 
				 socket.getOutputStream(), 
				 request);
		 } catch (IOException e) {
			 myLog.w("IOException in proxy sendRequest wrapper: " + e);
			 return null;
		 }
	}
	
	public ProxyDataSocketInfo pasvListen() {
		try {
			// connect to proxy and authenticate
			myLog.d("Sending data_pasv_listen to proxy");
			Socket socket = newAuthedSocket(this.hostname, Defaults.REMOTE_PROXY_PORT);
			if(socket == null) {
				myLog.w("pasvListen got null socket");
				return null;
			}
			JSONObject request = makeJsonRequest("data_pasv_listen");
			
			JSONObject response = sendRequest(socket, request);
			if(response == null) {
				return null;
			}
			int port = response.getInt("port");
			return new ProxyDataSocketInfo(socket, port);
		} catch(JSONException e) {
			myLog.l(Log.WARN, "JSONException in pasvListen");
			return null;
		}
	}
	
	public Socket dataPortConnect(InetAddress clientAddr, int clientPort) {
		/** 
		 * This function is called by a ProxyDataSocketFactory when it's time to
		 * transfer some data in PORT mode (not PASV mode). We send a 
		 * data_port_connect request to the proxy, containing the IP and port
		 * of the FTP client to which a connection should be made.
		 */
		try {
			myLog.d("Sending data_port_connect to proxy");
			Socket socket = newAuthedSocket(this.hostname, Defaults.REMOTE_PROXY_PORT);
			if(socket == null) {
				myLog.w("dataPortConnect got null socket");
				return null;
			}
			JSONObject request =  makeJsonRequest("data_port_connect");
			request.put("address", clientAddr.getHostAddress());
			request.put("port", clientPort);
			JSONObject response = sendRequest(socket, request);
			if(response == null) {
				return null; // logged elsewhere
			}
			return socket;
		} catch (JSONException e) {
			myLog.w("JSONException in dataPortConnect");
			return null;
		}
	}
	
	/**
	 * Given a socket returned from pasvListen(), send a data_pasv_accept request
	 * over the socket to the proxy, which should result in a socket that is ready
	 * for data transfer with the FTP client. Of course, this will only work if the
	 * FTP client connects to the proxy like it's supposed to. The client will have
	 * already been told to connect by the reponse to its PASV command. 
	 * 
	 * This should only be called from the onTransfer method of ProxyDataSocketFactory.
	 *  
	 * @param socket A socket previously returned from ProxyConnector.pasvListen()
	 * @return true if the accept operation completed OK, otherwise false
	 */
	
	public boolean pasvAccept(Socket socket) {
		try {
			JSONObject request = makeJsonRequest("data_pasv_accept");
			JSONObject response = sendRequest(socket, request);
			if(response == null) {
				return false;  // error is logged elsewhere
			}
			if(checkAndPrintJsonError(response)) {
				myLog.w("Error response to data_pasv_accept");
				return false;
			}
			// The proxy's response will be an empty JSON object on success
			myLog.d("Proxy data_pasv_accept successful");
			return true;
		} catch (JSONException e) {
			myLog.w("JSONException in pasvAccept: " + e);
			return false;
		}
	}
	
	public InetAddress getProxyIp() {
		if(this.isAlive()) {
			return commandSocket.getInetAddress();
		} else {
			return null;
		}
	}
	
	private JSONObject makeJsonRequest(String action) throws JSONException {
		JSONObject json = new JSONObject();
		json.put("action", action);
		return json;
	}
	
	public QuotaStats getQuotaStats(boolean canUseCached) {
		if(canUseCached) {
			if(cachedQuotaStats != null) {
				myLog.d("Returning cachedQuotaStats");
				return cachedQuotaStats;
			} else {
				myLog.d("Would return cached quota stats but none retrieved");
			}
		}
		// If there's no cached quota stats, or if the called wants fresh stats,
		// make a JSON request to the proxy, assuming the command session is open.
		try {
			JSONObject response = sendCmdSocketRequest(makeJsonRequest("check_quota"));
			int used, quota;
			if(response == null) {
				myLog.w("check_quota got null response");
				return null;
			}
			used = response.getInt("used");
			quota = response.getInt("quota");
			myLog.d("Got quota response of " + used + "/" + quota);
			cachedQuotaStats = new QuotaStats(used, quota) ;
			return cachedQuotaStats;
		} catch (JSONException e) {
			myLog.w("JSONException in getQuota: " + e);
			return null;
		}
	}
}

