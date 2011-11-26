#!/bin/bash
./bin/nbd-client bs=512 127.0.0.1 2000 testfs /dev/nbd0 100
mkfs /dev/nbd0
mount /dev/nbd0 /root/nbd0
