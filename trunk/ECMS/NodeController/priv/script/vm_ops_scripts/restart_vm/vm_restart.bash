#!/bin/bash
VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"
source $VM_OPS_SCRIPTS_DIR/utils/env.bash
source $VM_OPS_SCRIPTS_DIR/start_vm/scene.bash
function clean_all_ops(){
local VM_ID=$1
local HLFS_URI=$2
LOG_MSG "destroy vm if need"
xm des vm-$VM_ID >/dev/null 2>&1
sleep 2
LOG_MSG "unbind hlfs from nbd if need"
unbind_nbd_from_hlfs $HLFS_URI
}

LOG_MSG "restart vm ops begin..."

if [ $# != 2 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi  

VM_ID=$1
HLFS_URI=$2
VM_DIR=VM-$VM_ID
VM_NAME=vm-$VM_ID
SYSDISK=$VMS_WORK_DIR/$VM_DIR/sysdisk

LOG_MSG "step.0 do some check ..."
if xm domid $VM_NAME
then
   LOG_MSG "$VM_NAME has exist"
   exit 255
else
   LOG_MSG "$VM_NAME not exist , check pass"
fi

if ps -ef|grep nbd-client|grep -w $HLFS_URI
then
   LOG_MSG "$HLFS_URI has bind"
   exit 255
else
   LOG_MSG "HLFS_URL not exist ,check pass"
fi

if check_hlfs_is_exist $HLFS_URI
then
   LOG_MSG "$HLFS_URI is exist"
else
   LOG_MSG "$HLFS_URI not exist"
   exit 255
fi 

LOG_MSG "step.1 create workdir ...."
build_workdir_by_vmid $VM_ID
if [ $? -ne 0 ];then
   exit 255
fi

LOG_MSG "step.2 bind nbd to block device"
NBD=`find_unused_nbd_dev`
if [ $? -eq 0 ];then
   bind_nbd_to_hlfs $HLFS_URI $NBD
   if [ $? -ne 0 ];then
      clean_all_ops $VM_ID $HLFS_URI
      exit 255
   fi
else
   exit 255
fi
LOG_MSG "step.3 mount block device for sysdisk "
mkdir -p $SYSDISK
mount $NBD $SYSDISK >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "mount $BDEV $SYSDISK failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

LOG_MSG "step.4 start vm"
xm cr $SYSDISK/$VM_NAME.cfg
if [ $? -ne 0 ];then
   LOG_MSG " create vm failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

LOG_MSG "over"
