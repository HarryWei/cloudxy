#!/bin/bash
#source env.sh  #only for test
function create_snapshot_image(){
if [ $# != 2 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local SYSDISK=$1
local VM_OS_TYPE=$2
local BASEIMAGE=""
if [ $VM_OS_TYPE == "64bit" ];then
BASEIMAGE=$BASE_IMAGE_PATH/domU-64bit-FS.img
fi
 
if [ $VM_OS_TYPE == "32bit" ];then
BASEIMAGE=$BASE_IMAGE_PATH/domU-32bit-FS.img
fi 

if [ $BASEIMAGE == "" ]
then
   return 255
fi

local DIFFIMAGE=diff.img
vhd-util snapshot -n $SYSDISK/$DIFFIMAGE -p $BASEIMAGE -m >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "create snapshot failed"
   return 255;
fi

}
