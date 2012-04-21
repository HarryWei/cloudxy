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

LOG_MSG "start vm ops begin..."

if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
    echo "FAIL"
    exit 255
fi  

VM_ID=$1
VM_NAME=vm-$VM_ID
VM_DIR=VM-$VM_ID
SYSDISK=$VMS_WORK_DIR/$VM_DIR/sysdisk

LOG_MSG "step.0 do some check ..."
if xm domid $VM_NAME >/dev/null 2>&1
then
   LOG_MSG "$VM_NAME has exist"
   xm des $VM_NAME
   if [ $? -ne 0 ];then
      LOG_MSG "destroy vm:$VM_NAME failed"
      echo "FAIL"
      exit 255
   fi
else
   LOG_MSG "$VM_NAME not exist"
   #exit 1
fi

LOG_MSG "step.1 ubind nbd ..."
NBD=`mount -l|grep "\/$VM_DIR\/"|cut -d" " -f1`
if [ $? -ne 0 ];then
   LOG_MSG "can not find nbd device for vm:%$VM_NAME"
   echo "FAIL"
   exit 0
fi


sleep 3 #wait for tapdisk exit and release nbd
unbind_nbd $NBD
LOG_MSG "step.2 do blktap destroy for some xend bug hardcode fix?"
destroy_blktap_dev $SYSDISK/diff.img
LOG_MSG "over"
echo "SUCC"