#!/bin/bash
VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"
source $VM_OPS_SCRIPTS_DIR/utils/env.bash

function clean_all_ops(){
local VM_ID=$1
local HLFS_URI=$2
LOG_MSG "destroy hlfs space if need"
destroy_hlfs $HLFS_URI
}

LOG_MSG "start vm ops begin..."

if [ $# != 3 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi  

VM_ID=$1
HLFS_URI=$2
HLFS_DISK_SIZE=$3
VM_NAME=vm-$VM_ID
VM_DIR=VM-$VM_ID


LOG_MSG "step.0 do some check ..."
if xm domid $VM_NAME
then
   LOG_MSG "$VM_NAME has exist"
else
   LOG_MSG "$VM_NAME not exist , check failed"
   exit 255
fi


DEVID=`get_devid_by_hlfs $VM_NAME $HLFS_URI`
if [ $? -eq 0 ];then
   LOG_MSG "$HLFS_URI has been attach to vm:$VM_NAME as dev:$DEVID"
   exit 255
fi

LOG_MSG "step.1 mkfs hlfs ..."
mkfs_hlfs_with_size $HLFS_URI $HLFS_DISK_SIZE
if [ $? -ne 0 ];then
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

LOG_MSG "step.2 attach hlfs to vm"
XVD_DEV=`find_unused_dev $VM_NAME`
if [ $? -ne 0 ];then
   LOG_MSG "can not find any unused dev for vm:$VM_NAME"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi
xm block-attach $VM_NAME tap2:hlfs:$HLFS_URI $XVD_DEV w  
if [ $? -ne 0 ];then
   LOG_MSG "attach vdisk to vm failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi
LOG_MSG "over"
