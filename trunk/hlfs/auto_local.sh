#!/bin/bash
mkdir /tmp/testenv -p
cd ./build;make;cd -
cd ./build;./build_local.sh;cd -
modprobe nbd
umount /dev/nbd0
cd ./nbd-2.9.15/output/bin;./nbd-client -d /dev/nbd0;cd -
rm /tmp/nbd-* -rf
killall nbd-server
cd ./nbd-2.9.15/build;make clean;make;cd -
cd ./nbd-2.9.15/output/bin
./nbd-server 20000 & 
cd -
sleep 2
cd ./nbd-2.9.15/output/bin;./nbd-client bs=512 127.0.0.1 20000 local:///tmp/testenv/testfs /dev/nbd0;cd -
echo "OK ..."



