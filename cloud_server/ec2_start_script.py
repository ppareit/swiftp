#!/usr/bin/python
import sys
import os
import urllib

apps = [("/swiftp_proxy", "http://24.21.195.59/swiftp_proxy.tgz")]

logfilename = "/tmp/startup.log"
fh = open(logfilename, 'a')

def log(string):
    """Writes a string to both stdout and the log file"""
    fh.write(string)
    sys.stdout.write(string)

for i in range(0, len(apps)):
    directory, url = apps[i]
    
    log("Creating directory: " + directory)
    os.mkdir(directory)

    os.chdir(directory)
    
    log("Getting url: " + url)
    
    urllib.urlretrieve(url, './temp.tgz')

    wget $URL --output-document ./temp.tgz >> $LOGFILE

    if [ $? != "0" ]; then
        echo "Bailing out due to wget return value" >> $LOGFILE
        exit
    fi

    echo "Unpacking tgz" >> $LOGFILE
    tar -xvzf ./temp.tgz >> $LOGFILE

    if [ $? != "0" ]; then
        echo "Bailing out due to tar return value" >> $LOGFILE
        exit
    fi

    ./autostart.sh $APPS_TO_START >> $LOGFILE
done
