#!/bin/bash
#source env.sh  #only for test

function find_unused_nbd_dev(){
local arr=`ls /dev/nbd*`
for var in $arr
do 
  #echo $var
  df|grep -w $var >/dev/null 2>&1 
  if [ $? -ne 0 ];then
     echo $var
     return 0
  fi
done
return 255
}

function umount_nbd(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 1
fi
local NBD=$1
if mount -l|grep $NBD >/dev/null 2>&1
then
 umount $NBD >/dev/null 2>&1
 if [ $? -ne 0 ];then
   #LOG_MSG "umount for $BDEV failed"
   return 255;
 fi
fi
}

function bind_nbd_to_hlfs(){
if [ $# != 2 ]; then
   LOG_MSG  "$* parameter error";
   return 1
fi
local HLFS_URI=$1
local NBD=$2
$HLFS_TOOLS_DIR/nbd-client bs=512 127.0.0.1 $NBD_LISTION_PORT $HLFS_URI $NBD >/dev/null 2>&1 
if [ $? -ne 0 ];then
   #LOG_MSG "build block device for $HLFS_URI failed"
   return 255;
fi
} 

function unbind_nbd_from_hlfs(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 1
fi
local HLFS_URI=$1
if ps -ef|grep nbd-client|grep -w $HLFS_URI
then
  local NBD=`ps -ef|grep nbd-client|grep -w $HLFS_URI|awk '{print $13}'`
  LOG_MSG "check whether nbd:$NBD has be umounted first"
  umount_nbd $NBD
  if [ $? -ne 0 ];then
     LOG_MSG "umount for $NBD failed"
     return 255
  fi
  $HLFS_TOOLS_DIR/nbd-client -d $NBD
  if [ $? -ne 0 ];then
     LOG_MSG "nbd-client -d failed"
     return 255
  fi
fi
}

function unbind_nbd(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 1
fi
local NBD=$1
umount_nbd $NBD
if [ $? -ne 0 ];then
   LOG_MSG "umount for $NBD failed"
   return 255
fi
$HLFS_TOOLS_DIR/nbd-client -d $NBD >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "nbd-client -d failed"
   return 255
fi
}
