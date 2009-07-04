#!/usr/bin/python
import socket
import sys
import time

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

sock.connect(("localhost", 2222))

print "Connected."

inter_delay = sys.argv[1]
scripts = sys.argv[2:]

for i in range(0, len(scripts)):
    print "<<<Sleep " + inter_delay + ">>>"
    time.sleep(float(inter_delay))
    fh = open(scripts[i], "r")
    contents = fh.read().strip()
    fh.close()
    sock.send(contents)
    print "<<<Send>>>" + contents
    received = sock.recv(1024)
    print "<<<Recv>>>" + received

sys.stdin.read()

# Do nothing, give the user time to examine output

