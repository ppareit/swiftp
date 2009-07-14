#!/bin/bash

ADDRULECMD='iptables -t nat -A PREROUTING -p tcp --dport 21 -j REDIRECT --to-port 2121'

RULECHECKCMD="iptables -t nat -S PREROUTING | grep -- '-A PREROUTING -p tcp -m tcp --dport 21 -j REDIRECT --to-ports 2121'"

echo Checking for iptables redirect rule...

`$RULECHECKCMD`
if [ "$?" == "0" ] ; then
	echo Rule already installed, no action necessary.
else
	echo Rule not in place, installing...
	`$ADDRULECMD`
	echo Done installing rule.
fi

