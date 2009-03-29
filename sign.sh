#!/bin/bash

# This is a simple script to save typing when it's time to sign a release
# jar file. It assumes a number of things about the directory structure
# and the location of SwiFTP.apk.

jarsigner -keystore ../swiftp_private/swiftp.keystore ./SwiFTP.apk swiftp
jarsigner -verify -verbose -certs SwiFTP.apk
