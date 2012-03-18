#!/bin/bash
#source env.sh  #only for test
function get_domid_by_name(){
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VMNAME=$1
local VMID=`xm domid $VMNAME`
if [ $? -eq 0 ];then
   echo $VMID   
   return 0
fi
return 255

#VMID=0
#xm list > tmp.$$
#while read vmname vmid other
#do 
#   LOG_MSG $vmname $vmid $other
#   if [ $vmname == $1 ];then
#       LOG_MSG "find"
#       VMID=$vmid
#       break; 
#   fi
#done <tmp.$$
#rm -rf tmp.$$
#return $VMID
}

function get_vname_by_id(){
if [ $# != 1 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VMID=$1
local VMANME=`xm domname $VMID`
if [ $? -eq 0 ];then
   echo $VMNAME   
   return 0
fi
return 255
}


function create_vm_conf(){
if [ $# != 5 ]; then
    LOG_MSG  "$* parameter error";
    return 255
fi
local VMID=$1
local VMMEM=$2
local VMIP=$3
local VNCPASSWD=$4
local VNCPORT=$5
local DIFFIMAGE_PATH=$VMS_WORK_DIR/VM-$VMID/sysdisk/diff.img
cat <<EOF >$SYSDISK/vm-$VMID.cfg
name = "vm-$VMID"
memory = $VMMEM
vcpu = 1
bootloader="/usr/bin/pygrub"
disk = [ "tap2:vhd:$DIFFIMAGE_PATH,xvda1,w" ]
vif  = [ "bridge=xenbr0,script=vif-bridge,ip=$VMIP"]
vfb  = [ "type=vnc,vncunused=$((VNCPORT-5900)),vncpasswd=$VNCPASSWD" ]
EOF
}

#get_domid_by_name Domain-0

