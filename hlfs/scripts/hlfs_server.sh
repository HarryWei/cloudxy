#!/bin/bash
modprobe nbd
./bin/nbd-server 2000 local:///tmp/testenv
