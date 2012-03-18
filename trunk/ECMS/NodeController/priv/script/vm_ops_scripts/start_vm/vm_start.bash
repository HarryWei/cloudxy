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
LOG_MSG "destroy hlfs space if need"
destroy_hlfs $HLFS_URI
}

LOG_MSG "start vm ops begin..."

if [ $# != 9 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi  

VM_ID=$1
HLFS_URI=$2
VM_MEM=$3
VNC_PASSWD=$4
VNC_PORT=$5
VM_HOSTNAME=$6
VM_PASSWD=$7
VM_IPADDR=$8
VM_OS_TYPE=$9
VM_NAME=vm-$VM_ID
VM_DIR=VM-$VM_ID
ISO_FILE_PATH=$VMS_WORK_DIR/$VM_DIR/vm-$VM_ID-scene.iso
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


LOG_MSG "step.1 create workdir ...."
build_workdir_by_vmid $VM_ID
if [ $? -ne 0 ];then
   exit 255
fi

LOG_MSG "step.2 mkfs hlfs ..."
mkfs_hlfs $HLFS_URI
if [ $? -ne 0 ];then
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi


lock_req /tmp/nbd-use-lock
LOG_MSG "step.3 bind nbd to block device"
NBD=`find_unused_nbd_dev`
if [ $? -eq 0 ];then
   bind_nbd_to_hlfs $HLFS_URI $NBD
   if [ $? -ne 0 ];then
      clean_all_ops $VM_ID $HLFS_URI
      lock_release /tmp/nbd-use-lock
      exit 255
   fi
else
   lock_release /tmp/nbd-use-lock
   exit 255
fi


LOG_MSG "step.4 mkfs block device for contain sysdisk"
mkfs.ext3 -T largefile $NBD >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "mkfs for $NBD failed"
   clean_all_ops $VM_ID $HLFS_URI
   lock_release /tmp/nbd-use-lock
   exit 255
fi


LOG_MSG "step.5 mount block device for sysdisk "
mkdir -p $SYSDISK
mount $NBD $SYSDISK >/dev/null 2>&1
if [ $? -ne 0 ];then
   LOG_MSG "mount $BDEV $SYSDISK failed"
   clean_all_ops $VM_ID $HLFS_URI
   lock_release /tmp/nbd-use-lock
   exit 255
fi
lock_release /tmp/nbd-use-lock

LOG_MSG "step.6 create snapshot image for sysdisk"
create_snapshot_image $SYSDISK $VM_OS_TYPE
if [ $? -ne 0 ];then
   LOG_MSG " create snapshot image failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi


LOG_MSG "step.7 create vm config "
create_vm_conf $VM_ID $VM_MEM $VM_IPADDR $VNC_PASSWD $VNC_PORT
if [ $? -ne 0 ];then
   LOG_MSG " create vm conf failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

LOG_MSG "step.8 start vm"
xm cr $SYSDISK/$VM_NAME.cfg
if [ $? -ne 0 ];then
   LOG_MSG " create vm failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

LOG_MSG "step.9 make scene for custom"

mk_scene_iso $VM_NAME $VM_HOSTNAME $VM_PASSWD $VM_IPADDR $ISO_FILE_PATH 
if [ $? -ne 0 ];then
   LOG_MSG "build scene_iso file failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

attach_iso_to_vm $VM_NAME $ISO_FILE_PATH
if [ $? -ne 0 ];then
   LOG_MSG "attach iso to vm failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

notify_vm_iso_attached $VM_NAME
if [ $? -ne 0 ];then
   LOG_MSG "notify vm iso attached failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi

wait_vm_scene_over $VM_NAME 
if [ $? -ne 0 ];then
   LOG_MSG "wait vm scene over failed"
   clean_all_ops $VM_ID $HLFS_URI
   exit 255
fi
detach_iso_from_vm $VM_NAME
LOG_MSG "over"
