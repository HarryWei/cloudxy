#!/bin/bash

#VMS_WORK_DIR="/opt/vms_work_dir"
#VM_OPS_LOG_DIR="/var/log/vm"
#VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"
#HLFS_TOOLS_DIR="/opt/hlfs_tools"
#BASE_IMAGE_PATH="/opt/baseimage"
#NBD_LISTION_PORT=20000

function mkfs_hlfs(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local HLFS_URI=$1
$HLFS_TOOLS_DIR/mkfs.hlfs -u $HLFS_URI -b 8192 -s 67108864 -m 512 >/dev/null 2>&1
local res=$?
if [ $res -eq 0 ];then
   LOG_MSG "mkfs.hlfs for $HLFS_URI success"
   return 0
fi
if [ $res -eq 1 ];then
   LOG_MSG "$HLFS_URI has exist"
   return 1;
fi
return 255
}

function mkfs_hlfs_with_size(){
if [ $# != 2 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local HLFS_URI=$1
local HLFS_DISK_SIZE=$2
$HLFS_TOOLS_DIR/mkfs.hlfs -u $HLFS_URI -b 8192 -s 67108864 -m $HLFS_DISK_SIZE >/dev/null 2>&1
local res=$?
if [ $res -eq 0 ];then
   LOG_MSG "mkfs.hlfs for $HLFS_URI success"
   return 0
fi
if [ $res -eq 1 ];then
   LOG_MSG "$HLFS_URI has exist"
   return 1;
fi
return 255
}

#hdfs://192.168.0.1/tmp/testenv/testfs
#local:///tmp/testenv/testfs
function check_hlfs_is_exist(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local HLFS_URI=$1
local MODE=`echo $HLFS_URI|cut -d":" -f1`
local OTHER=`echo $HLFS_URI|cut -d":" -f2`
if [ $MODE == "hdfs" ];then
   LOG_MSG "it is hdfs mode"
   if hadoop fs -test -e $HLFS_URI
   then
        return 0;
   else
        return 255;
   fi 
else if [ $MODE == "local" ];then
        LOG_MSG "it is local mode"
        if test -e $OTHER
        then 
             return 0;
        else
             return 255;
        fi
     else
        LOG_MSG "it is illege mode:$MODE"
        return 255 
     fi
fi
}

function destroy_hlfs(){
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
   return 255
fi
local HLFS_URI=$1
local MODE=`echo $HLFS_URI|cut -d":" -f1`
local OTHER=`echo $HLFS_URI|cut -d":" -f2`
if [ $MODE == "hdfs" ];then
   LOG_MSG "it is hdfs mode"
   if hadoop fs -rmr  $HLFS_URI
        then
           return 0;
        else
           return 255;
   fi 
else if [ $MODE == "local" ];then
        LOG_MSG "it is local mode"
        if rm -rf $OTHER
        then 
            return 0;
        else
            return 255;
        fi
     else
        LOG_MSG "it is illege mode:$MODE"
        return 255 
     fi
fi
}


#mkfs_hlfs local:///tmp/testenv/testfs
#check_hlfs_is_exist local:///tmp/testenv/testfs
#destroy_hlfs local:///tmp/testenv/testfs
