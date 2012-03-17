#!/bin/bash
VMS_WORK_DIR="/opt/vms_work_dir"
VM_OPS_LOG_DIR="/var/log/vm"
VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"
HLFS_TOOLS_DIR="/opt/hlfs_tools"
BASE_IMAGE_PATH="/opt/baseimage"
NBD_LISTION_PORT=20000
init_env(){
if [ ! -d $VMS_WORK_DIR ];then
  mkdir -p $VMS_WORK_DIR >/dev/null 2>&1;
fi
if [ ! -d $VM_OPS_LOG_DIR ];then
  mkdir -p $VM_OPS_LOG_DIR >/dev/null 2>&1;
fi
if [ ! -d $VM_OPS_SCRIPTS_DIR ];then
  LOG_MSG "not find vm ops scripts dir"
  return 255
fi
if [ ! -d $HLFS_TOOLS_DIR ];then
  LOG_MSG "not find hlfs tools"
  return 255
fi
if [ ! -d $BASE_IMAGE_PATH ];then
  LOG_MSG "not find base iamge path"
  return 255
fi

if netstat -nlpt|grep nbd-server >/dev/null 2>&1
then
   LOG_MSG "ndb server has launch"
else
   LOG_MSG  "nbd server has not lauch"
   modprobe nbd
   rm -rf /tmp/nbd* > /dev/null 2>&1 
   $HLFS_TOOLS_DIR/nbd-server $NBD_LISTION_PORT >/dev/null 2>&1 &
   if [ $? -eq 0 ];then
   	LOG_MSG  "nbd server has been lauched"
   else
   	LOG_MSG  "nbd server lauche failed"
        return 255
   fi 
fi
}
source $VM_OPS_SCRIPTS_DIR/utils/log.bash
source $VM_OPS_SCRIPTS_DIR/utils/misc.bash
source $VM_OPS_SCRIPTS_DIR/utils/hlfs_ops.bash
source $VM_OPS_SCRIPTS_DIR/utils/nbd_ops.bash
source $VM_OPS_SCRIPTS_DIR/utils/vm_ops.bash
source $VM_OPS_SCRIPTS_DIR/utils/vhd_ops.bash
source $VM_OPS_SCRIPTS_DIR/utils/vdisk_ops.bash
init_env
