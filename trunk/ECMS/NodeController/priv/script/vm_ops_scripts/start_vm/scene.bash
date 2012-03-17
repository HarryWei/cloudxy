#!/bin/bash
#source env.sh  #only for test

function attach_iso_to_vm()
{
if [ $# != 2 ]; then
   LOG_MSG  "$* parameter error";
return 255
fi 

local VMNAME=$1
local ISO_FILE_PATH=$2
#local FIND=0

xm domname $VMNAME
if [ $? -ne 0 ];then
   LOG_MSG "not find VM:$VMNAME"
   return 255
fi

xm block-attach $VMNAME file:$ISO_FILE_PATH hdd:cdrom r >/dev/null 2>&1
if [ $? -eq 0 ];then
   return 0;
else
   LOG_MSG "attach cdrom to $VMNAME failed"
   return 255;
fi

#xm list | while read vmid other
#xm list > tmp.$$
#while read vmname other
#do 
#   LOG_MSG $vmid $other
#   if [ $vmname == $vm_name ];then
#       LOG_MSG "find"
#       FIND=1
#       LOG_MSG $FIND
#       break 
#   else
#       LOG_MSG "not find"
#   fi
#done <tmp.$$
#rm -rf tmp.$$
#LOG_MSG "find... $FIND"
#if [ $FIND == 1 ];then
#  LOG_MSG "find it"
#  xm block-attach $vm_name file:`pwd`/$scene_iso_file hdc:cdrom r >/dev/null 2>&1
#  if [ $? -eq 0 ];then
#    return 0;
#  else
#    LOG_MSG "attach cdrom to $vm_name failed"
#    return 255;
#  fi
#else
#  LOG_MSG "can not find $vmid"
#  return 255
#fi
}

function detach_iso_from_vm()
{
if [ $# != 1 ]; then
   LOG_MSG  "$* parameter error";
return 255
fi 
local VMNAME=$1
xm domname $VMNAME
if [ $? -ne 0 ];then
   LOG_MSG "not find VM:$VMNAME"
   return 255
fi

xm block-detach $VMNAME /dev/hdd -f >/dev/null 2>&1
if [ $? -eq 0 ];then
   return 0;
else
   LOG_MSG "detach cdrom from $vm_name failed"
   return 255;
fi
}

function notify_vm_iso_attached(){ 
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
return 255
fi
local VMNAME=$1
#get_domid_by_name $vm_name
local VMID=`xm domid $VMNAME`
if [ $? -ne 0 ];then
   LOG_MSG "can not find vmid by name:$VMNAME"
   return 255
fi
xenstore-write /local/domain/$VMID/iso_attach 1
if [ $? -ne 0 ];then
   LOG_MSG "can not set vmid:$VMID 's iso_attach field"
   return 255;
fi
xenstore-chmod /local/domain/$VMID/iso_attach b
if [ $? -ne 0 ];then
   LOG_MSG "can not chmod vmid:$VMID's iso_attach field"
   return 255;
fi
}

function wait_vm_scene_over(){
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
return 255
fi
local VMNAME=$1
local VMID=`xm domid $VMNAME`
if [ $VMID -eq 0 ];then
   return 255
fi
local counter=0
while :; do
    LOG_MSG "check dom $VMID ios_attach field"
    local flag=`xenstore-read /local/domain/$VMID/iso_attach`
    if [ $flag == '2' ];then
        LOG_MSG "domU has scene over"
        break;
    fi
    sleep 1
    counter=`expr $counter + 1`
    if [ $counter -eq 10 ];then
        LOG_MSG "domU scene over timeout"
        return 255;
    fi
    sleep 1
done
}

function mk_scene_iso()
{
if [ $# != 5 ];then
LOG_MSG "$* parameter error"
return 255
fi
local vm_id=$1
local vm_hostname=$2
local vm_passwd=$3
local vm_ipaddr=$4
local scene_iso_file=$5
local scene_file=`dirname $scene_iso_file`/initialize
echo "hostname $vm_hostname" > $scene_file;
echo "password $vm_passwd" >> $scene_file;
echo "ipaddr $vm_ipaddr" >> $scene_file;
#cat $scene_file
mkisofs -r -o $scene_iso_file $scene_file >/dev/null 2>&1;
if [ $? != 0 ]; then
LOG_MSG  "mkisofs error";
return 255;
fi
}
