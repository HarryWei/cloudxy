#!/bin/bash
## update time 2011-7-26
## VcpuCount,MemSize,DiskSize,IpAddr,MacAddr,VncPass,VncPort,VmUser,VmUserPass,VmI
## argument 2 64 64 "192.168.3.120" "11:22:33:44:55:66" 123456 5901 jiangang 123456 1

NUM=$#
so_config='/Xen/iso/'
if [ $NUM != 10 ];then 
        echo "FAIL"
        exit 1
fi
VMVCPU=$1
VMMEM=$2
VMDISKSIZE=$3
VMIP=$4
VMMAC=$5
VNCPASSWD=$6
VNCPORT=$7
USER=$8
USERPASSWD=$9
VMID=${10}

pushd /Xen/iso/  >/Xen/log/${10}  2>&1
touch $USER
echo $USER >> $USER
echo $USERPASSWD >> $USER
popd  >/dev/null 2>&1
mkisofs -o /tmp/$USER.iso /Xen/iso > /Xen/log/${10} 1> /dev/null  2>&1
rm -f /Xen/iso/$USER
vhd-util snapshot -p /Xen/image/raw/raw-mother.img -n /Xen/image/vhd/$VMID.img -m  >/Xen/log/${10}  2>&1
pushd /Xen/config-of-VM  >/Xen/log/${10}  2>&1
cat << EOF >$USER.cfg
memory = $VMMEM
name = "$VMID"
vcpu = $VMVCPU
on_poweroff = "restart"
on_reboot = "restart"
on_crash = "restart"
bootloader="/usr/bin/pygrub"
disk = [ "tap2:vhd:/Xen/image/vhd/$VMID.img,xvda,w" ]
vif = ["bridge=xenbr0,script=vif-bridge,ip=$VMIP,mac=$VMMAC"]
vfb = [" type=vnc,vncunused=$((VNCPORT-5900)),vncpasswd=$VNCPASSWD" ]
EOF
popd  >/dev/null 2>&1
echo "SUCC"
