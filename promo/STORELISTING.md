# Store listing

## App name

FTP Server: SwiFTP

## Short description

Turn your Android device into an FTP server for easy file transfers.

## Full description

This program allows you to run an ftp server on your android device. This means that any other computer/device can access the files on your android device while the ftp server is running. For example, entering 'ftp://...' in most file explorers or FTP clients will allow you to browse, upload, download, rename, create, and delete files and folders.

SwiFTP can provide FTP access across your device's shared storage, or you can restrict access to specific folders. Full shared-storage access enables complete FTP file management, including operations that Android's selected-folder access does not support.

The default username and password are both "ftp". Change them before using the server. For security and power-saving reasons, it is recommended that the server be stopped after use. Though, cpu and memory use is minimal. It is easy to see and stop the server directly from the notification.

Features:

- Complete FTP server implemented in the app itself
- Full FTP file management across shared storage: browse, upload, download, create, rename, and delete files and folders
- Choose between full shared-storage access (Android's "All files access") or access to only the folders you select (Android's "Storage Access Framework")
- Read and write files in internal shared storage and external storage
- FTP features including UTF-8, MDTM, and MFMT
- Encrypted FTPS connections, both explicit and implicit modes are supported
- Bonjour/DNS-SD service discovery for compatible clients
- Multiple user accounts with option to have individual home folders
- Optional anonymous login with read-only access
- Access to all shared storage or only the folders you select
- Per-user folder restrictions (chroot)
- Configurable server and passive-mode ports, the default FTP port is 2121
- IP allow and deny rules, with protection against repeated failed logins
- Support for local networks, including Wi-Fi, Ethernet, hotspots, and tethering
- Option to keep the server available while the screen is off
- Tasker and Locale plug-in support to start or stop the server, check its state, and react to state changes (contact me for the plug-in support)
- Start and stop controls in the app, persistent notification, home-screen widget, Quick Settings tile, and launcher shortcut
- Interface designed for phones, tablets, and TVs
- Optional logging for troubleshooting

The server is completely implemented in the app itself, it does not use an external library. It provides the best possible performance on android to run. Some advanced features like UTF8, MDTM and MFMT are implemented. Though the underlying file system must support them.

The Bonjour/DNS-SD support is very handy if the client os and it's file manager also supports the protocols. In this way, the moment you start the ftp server on the android device, you will find it on the network folder of your desktop.

FTP Server is open source software released under the GPL v3.
Source Code: https://github.com/ppareit/swiftp
Issues Tracker: https://github.com/ppareit/swiftp/issues

Current maintainer: Pieter Pareit  
Initial development: Dave Revell
