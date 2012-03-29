#!/bin/bash
VMS_WORK_DIR="/opt/vms_work_dir"
VM_OPS_LOG_DIR="/var/log/vm"
VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"

if [ `getconf LONG_BIT` == "64" ];then
HLFS_TOOLS_DIR="/opt/hlfs_tools/bin64"
#export LD_LIBRARY_PATH=/opt/hlfs_tools/lib64
#mkdir -p /usr/lib/hlfs
ldconfig -p |grep /opt/hlfs_tools/lib64 1>/dev/null 2>&1
if [ $? -ne 0 ];then
echo "/opt/hlfs_tools/lib64" >> /etc/ld.so.conf
ldconfig
fi
fi
if [ `getconf LONG_BIT` == "32" ];then
HLFS_TOOLS_DIR="/opt/hlfs_tools/bin32"
#export LD_LIBRARY_PATH=/opt/hlfs_tools/lib32
ldconfig -p |grep /opt/hlfs_tools/lib32 1>/dev/null 2>&1
if [ $? -ne 0 ];then
echo "/opt/hlfs_tools/lib32" >> /etc/ld.so.conf
ldconfig
fi
fi

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
  else
  chmod +x $HLFS_TOOLS_DIR/*
fi
if [ ! -d $BASE_IMAGE_PATH ];then
  LOG_MSG "not find base iamge path"
  return 255
fi

if netstat -nlpt|grep nbd-server >/dev/null 2>&1
then
   LOG_MSG "ndb server has launch"
   modprobe nbd > /dev/null 2>&1
else
   LOG_MSG  "nbd server has not lauch"
   modprobe nbd
   rm -rf /tmp/nbd* > /dev/null 2>&1 
   $HLFS_TOOLS_DIR/nbd-server $NBD_LISTION_PORT >/dev/null 2>&1 &
   sleep 1
   netstat -nlpt|grep nbd-server >/dev/null 2>&1
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
