#!/bin/bash

jarsigner -keystore ../swiftp_private/swiftp.keystore ./SwiFTP.apk swiftp
jarsigner -verify -verbose -certs SwiFTP.apk
