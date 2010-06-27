#!/usr/bin/python
import sys
import os
import urllib

# Config section
apps = [("/swiftp_proxy", "http://24.21.195.59/swiftp_proxy_dev.tgz", "-detached -jointo ''")]
do_apt_upgrade = False


# Code section

logfilename = "/tmp/startup.log"
fh = open(logfilename, 'a')

def log(string):
    """Writes a string to both stdout and the log file"""
    string += "\n"
    fh.write(string)
    sys.stdout.write(string)


if do_apt_upgrade:
    log("Doing apt upgrade...")
    os.system("apt-get update && apt-get -y upgrade")

for i in range(0, len(apps)):
    directory, url, autostart_args = apps[i]
    
    log("Creating directory: " + directory)
    if not os.access(directory, os.F_OK):
        # Create target directory if it doesn't exist
        os.mkdir(directory)
        
    if not os.access(directory, os.R_OK | os.R_OK):
        log("Can't access directory, skipping: " + directory)
        continue

    os.chdir(directory)
    if os.getcwd() != directory:
        log("chdir seemed to fail, skipping URL: " + url)
        continue
        
    log("Getting url: " + url)
    urllib.urlretrieve(url, './temp.tgz')
    if not os.access('./temp.tgz', os.R_OK):
        log("Failed to retrieve URL, skipping: " + url)
        continue

    log("Unpacking tgz...")
    os.system('tar -xvzf ./temp.tgz')

    if not os.access('./autostart', os.R_OK | os.X_OK):
        log("autostart did not exist after unpacking TGZ, skipping: " + url)
    else:
        log("Running autostart script...")
        os.system('./autostart' + ' ' + autostart_args)

log("Finished, exiting.")
