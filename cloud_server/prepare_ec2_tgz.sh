#!/bin/bash

erlc *.erl

if [ "$?" != "0" ]
then
	echo "Compilation failed, stopping."
	exit
fi

if [ "$1" != "-dev" ]
then
	DATE=`date +'%y%m%d_%H%M%S'`
	FILE="/var/www/swiftp_proxy_$DATE.tgz"
else
	FILE="/var/www/swiftp_proxy_dev.tgz"
fi

sudo tar -cz --exclude='*.svn' --exclude='*~' -f $FILE ./

if [ "$?" == "0" ]
then
	echo File ready: $FILE
else
	echo Error in tar, file not ready.
fi
