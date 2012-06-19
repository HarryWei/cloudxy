#!/bin/bash
#VM_OPS_SCRIPTS_DIR="/opt/vm_ops_scripts"
#source $VM_OPS_SCRIPTS_DIR/utils/env.bash
function get_devid_by_hlfs(){
if [ $# != 2 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VM_NAME=$1
local HLFS_URI=$2
xm block-list $VM_NAME|awk 'NR>1{print $7}'|while read key
do
   xenstore-ls $key | grep -v xvda1|grep -w $HLFS_URI >/dev/null 2>&1
   if [ $? -eq 0 ];then
       LOG_MSG "$HLFS_URI has attach to vm:$VM_NAME"
       basename $key
       return 0
   fi
done
return 255
}

function get_dev_by_devid(){
if [ $# != 2 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VMNAME=$1
local DEVID=$2
if DOMID=`xm domid $VMNAME` 
then
   xenstore-ls /local/domain/0/backend/vbd/$DOMID/$DEVID|grep -w "dev ="|cut -d"=" -f2|sed 's/\"//g'|sed 's/^[ \t]*//g' 
   if [ $? -eq 0 ];then
      echo $VMNAME
      return 0;
   fi
else
   LOG_MSG "not find vm:$VMNAME"
   return 255
fi 
return 255
}

function find_unused_dev(){
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VMNAME=$1

if ! DOMID=`xm domid $VMNAME`
then
   LOG_MSG "no such vm:$VMNAME"
   return 255
fi 
arr=(b c)
for i in ${arr[*]}
do 
   xenstore-ls /local/domain/0/backend/vbd/$DOMID|grep -w "dev = xvd$i" 
   if [ $? -ne 0 ];then
      echo xvd$i
      return 0
   fi
done
return 255
}

function destroy_blktap_dev(){
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local URI=$1
tap-ctl list|grep -w $URI
if [ $? -ne 0 ]; then
    LOG_MSG  "can not find dev:$URI";
    return 255
fi
PID=`tap-ctl list|grep -w local:///tmp/testenv/datafs83|cut -d" " -f1|cut -d"=" -f2`
MINOR=`tap-ctl list|grep -w local:///tmp/testenv/datafs83|cut -d" " -f2|cut -d"=" -f2`
tap-ctl destroy -p $PID -m $MINOR
if [ $? -ne 0 ]; then
    LOG_MSG  "can not destroy dev:$URI";
    return 255
fi
}

#devid=`get_devid_by_hlfs vm-77 local:///tmp/testenv/testfs`
#get_dev_by_devid vm-77 $devid
#find_unused_dev vm-77
