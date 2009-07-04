#!/bin/bash

APPURLS=("http://24.21.195.59/swiftp_proxy.tgz")
APPDIRECTORIES=("/swiftp_proxy")


LOGFILE="/tmp/startup.log"
UNPACKED_DIR="/dl_root"

###

NUMURLS=${#APPURLS[*]}

for (( I = 0 ; I <= $NUMURLS ; I++ ))
do
    URL=$APPURLS[$I]
    DIRECTORY=$APPDIRECTORIES[$I]
    
    mkdir $DIRECTORY
    cd $DIRECTORY
    
    if [ $URL = "" ]; then
        echo "You forgot to specify a destination URL" >> $LOGFILE
        exit
    fi

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
