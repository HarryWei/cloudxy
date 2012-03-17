#!/bin/bash
#source env.sh  #only for test
function create_snapshot_image(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local SYSDISK=$1
local BASEIMAGE=$BASE_IMAGE_PATH/domU-64bit-FS.img
local DIFFIMAGE=diff.img
vhd-util snapshot -n $SYSDISK/$DIFFIMAGE -p $BASEIMAGE -m >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "create snapshot failed"
   return 255;
fi
}
