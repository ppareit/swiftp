// SPDX-License-Identifier: GPL-3.0-or-later

package be.ppareit.swiftp.utils

import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.util.Log

import be.ppareit.swiftp.App

import java.net.Inet4Address
import java.net.InetAddress
import java.net.NetworkInterface
import java.net.SocketException
import java.util.Collections

/**
 * Answers if is there a network a client could reach this device over?
 *
 * An interface that is up and carries a site local IPv4 address is one a client on that
 * network can connect to, whatever it is called, so a hotspot and a tethered link count as
 * readily as wifi. Only mobile data is ruled out, because a carrier hands out site local
 * addresses too and only ConnectivityManager tells the two apart.
 */
object LocalNetwork {

    private val TAG = LocalNetwork::class.java.simpleName

    /**
     * Mobile data, under the names the common modem drivers use. A fallback for a link
     * ConnectivityManager does not report; the name alone is not enough, an emulator calls
     * its cellular link eth0.
     */
    private val MOBILE = Regex("^(rmnet|ccmni|pdp).*")

    /** The interfaces ConnectivityManager says carry mobile data. */
    private fun cellularInterfaces(): Set<String> {
        val cm = App.getAppContext().getSystemService(ConnectivityManager::class.java)
            ?: return emptySet()
        val names = mutableSetOf<String>()
        // Every network rather than the default one: while wifi is up mobile data is not the
        // default, and that is exactly when it must not be mistaken for a LAN.
        @Suppress("DEPRECATION")
        for (network in cm.allNetworks) {
            val capabilities = cm.getNetworkCapabilities(network)
                ?: continue
            if (!capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR))
                continue
            cm.getLinkProperties(network)?.interfaceName?.let { names.add(it) }
        }
        return names
    }

    /**
     * @return true when at least one interface can carry an FTP session
     */
    @JvmStatic
    fun isAvailable(): Boolean {
        val serveable = serveableInterfaces()
        Log.d(TAG, "Can serve on $serveable")
        return serveable.isNotEmpty()
    }

    /**
     * True when mobile data is the only thing up. Nobody can reach the server there.
     *
     * This is a refusal a user is likely to read as a bug: the phone plainly has a working
     * connection. Thus the UI needs to show information about this!
     *
     * Asked only after [isAvailable] has said no.
     */
    @JvmStatic
    fun onlyMobileDataIsUp(): Boolean {
        val reachable = interfaces().filter { it.isReachableLink() }
        return reachable.isNotEmpty() && reachable.all { isMobile(it.name) }
    }

    /** Every interface a client could reach us over, in enumeration order. */
    private fun serveableInterfaces(): List<String> =
        interfaces().filter { it.canServe() }.map { it.name }

    private fun interfaces(): List<NetworkInterface> = try {
        Collections.list(NetworkInterface.getNetworkInterfaces())
    } catch (e: SocketException) {
        Log.w(TAG, "Unable to enumerate the network interfaces", e)
        emptyList()
    }

    /** This is the nice way to use canServe, on NetworkInterface, but untestable */
    private fun NetworkInterface.canServe(): Boolean = try {
        canServeOn(
            name, isUp, isLoopback,
            Collections.list(inetAddresses), cellularInterfaces()
        )
    } catch (e: SocketException) {
        Log.w(TAG, "Skipping $name, it cannot be queried", e)
        false
    }

    /** The same, for the link itself, whatever it is named. */
    private fun NetworkInterface.isReachableLink(): Boolean = try {
        isReachableLink(isUp, isLoopback, Collections.list(inetAddresses))
    } catch (e: SocketException) {
        Log.w(TAG, "Skipping $name, it cannot be queried", e)
        false
    }

    /** Here is the real canServe, without NetworkInterface, so it is testable  */
    @JvmStatic
    fun canServeOn(
        name: String?, isUp: Boolean, isLoopback: Boolean,
        addresses: List<InetAddress>, cellular: Set<String>
    ): Boolean = isReachableLink(isUp, isLoopback, addresses) && !isMobile(name, cellular)

    /**
     * A link a client could open a connection over, before asking what it is called. Mobile data
     * passes this and still cannot serve, which is what separates the two refusals.
     */
    @JvmStatic
    fun isReachableLink(isUp: Boolean, isLoopback: Boolean, addresses: List<InetAddress>): Boolean {
        if (!isUp || isLoopback) return false
        return addresses.any { isServeableAddress(it) }
    }

    @JvmStatic
    fun isMobile(name: String?, cellular: Set<String>): Boolean =
        name != null && (cellular.contains(name) || MOBILE.matches(name))

    /** The same, asking the platform which interfaces are cellular. */
    private fun isMobile(name: String?): Boolean = isMobile(name, cellularInterfaces())

    /**
     * A LAN address a client can dial. Site local IPv4 only: link local (169.254) means the
     * interface never got a lease, and the data channel needs an IPv4 address to put in a PASV
     * reply either way.
     */
    @JvmStatic
    fun isServeableAddress(address: InetAddress): Boolean =
        address is Inet4Address
                && address.isSiteLocalAddress
                && !address.isLoopbackAddress
                && !address.isLinkLocalAddress
}
