#!/bin/bash


#File name: vm_start.sh
#Author: Harry Wei <harryxiyou@gmail.com> 2012 2 19
#Program descripton: This script starts a xen virtual machine
#Change Log: 2012-2-19 harry Created this script

FILEPATH="$PWD/vm_start.sh"
LOGFILE=/tmp/vm_ops.log

#This function print error logs to the file $LOGFILE
log() {
	echo "[`date "+%Y/%m/%d %H:%M:%S"` $FILEPATH] "$*"" >> $LOGFILE
}

#This function just prints the script's usage
usage() {
	echo "Usage: $0 -i <vm_id> -c <vm_cpu_count> -s <vm_mem_size> -p <vm_passwd> -h <vm_hostname> -a <vm_ip_addr> -m <vm_mac_addr> -o <vnc_port> -w <vnc_passwd> -t <vm_os_type>"
}

debug() {
	echo "vm_id is $vm_id"
	echo "vm_cpu_count is $vm_cpu_count"
	echo "vm_mem_size is $vm_mem_size"
	echo "vm_passwd is $vm_passwd"
	echo "vm_hostname is $vm_hostname"
	echo "vm_ip_addr is $vm_ip_addr"
	echo "vm_mac_addr is $vm_mac_addr"
	echo "vnc_port is $vnc_port"
	echo "vnc_passwd is $vnc_passwd"
	echo "vm_os_type is $vm_os_type"
}

if [ $# != 20 ]; then
	usage;
	log "Parameters Error";
	exit 1;
fi

while getopts i:c:s:p:h:a:m:o:w:t: option
do
	case "$option" in
		i) vm_id=$OPTARG;;
		c) vm_cpu_count=$OPTARG;;
		s) vm_mem_size=$OPTARG;;
		p) vm_passwd=$OPTARG;;
		h) vm_hostname=$OPTARG;;
		a) vm_ip_addr=$OPTARG;;
		m) vm_mac_addr=$OPTARG;;
		o) vnc_port=$OPTARG;;
		w) vnc_passwd=$OPTARG;;
		t) vm_os_type=$OPTARG;;
		?) usage; log "Parameters Error"; exit 1;;
	esac
done

debug;

#Step 1. Check if the dir exists. if so, delete it and 
#recreate it. if not, create it directly.
work_dir="$HOME/$vm_id"
if [ -d $work_dir ]; then
		if rm -rf $work_dir		1>/dev/null 2>&1
		then 
			echo "Remove dir successfully";
			cd $HOME;		1>/dev/null 2>&1
			mkdir $vm_id;	1>/dev/null 2>&1
			cd $vm_id;		1>/dev/null 2>&1
		else
			echo "Fail to remove dir, check your dir's permission!";
			log "Fail to remove dir, check your dir's permission!";
		fi
else
	mkdir $work_dir;	1>/dev/null 2>&1
	cd $work_dir;		1>/dev/null 2>&1
fi

#Step 2. Check if first start (Judge if sys_disk exist).
#If so, do step 3 in sequnce. If not, jump to step 7.
#Hlfs local URI pattern "local:///$HOME/sysdisk"
#Hlfs hdfs URI pattern "hdfs:///$HOME/sysdisk"
sysdisk_dir="$HOME/sysdisk"
sysname="$vm_id.sys.img"
if [ -f $sysdisk_dir/$sysname ]; then 
	echo "Jump to Step 7";
fi

#Step 3. Make iso image for vm_passwd and vm_hostname
scene_file="/tmp/initialize"
scene_iso_file="$HOME/$vm_id/$vm_id.iso"
echo "hostname: $vm_hostname" > $scene_file		1>/dev/null 2>&1
echo "password: $vm_passwd" >> $scene_file		1>/dev/null 2>&1
mkisofs -o $scene_iso_file $scene_file			1>/dev/null	2>&1
if [ $? != 0 ]; then
	echo "mkisofs error";
	log "mkisofs error";
	exit 1;
fi

#Step 4. Create system disk. TODO support *hdfs* pattern
tools_dir="$HOME/tools"
mkfs_hlfs="$tools_dir/mkfs.hlfs"
if [ -d $sysdisk_dir ]; then
		echo "$sysdisk_dir exist"
else
	mkdir $sysdisk_dir		1>/dev/null 2>&1
fi
if [ -f $mkfs_hlfs ]; then
	if [ -d $sysdisk_dir/$vm_id ]; then
		if rm -rf $sysdisk_dir/$vm_id		1>/dev/null 2>&1
		then
			echo "remove $sysdisk_dir/$vm_id successfully";
			log "remove $sysdisk_dir/$vm_id successfully";
		else
			echo "fail to remove $sysdisk_dir/$vm_id, check permission";
			log "fail to remove $sysdisk_dir/$vm_id, check permission";
		fi	
	fi
	$mkfs_hlfs -u local:///$sysdisk_dir/$vm_id -b 8192 -s 67108864 -m 1024	1>/dev/null 2>&1
	if [ $? != 0 ]; then
		echo "Format block device error";
		log "Format block device error";
		exit 1;
	fi
	tap-ctl create -a hlfs:local:///$sysdisk_dir/$vm_id		1>/dev/null 2>&1
	tap_ctl_ret=$?
	if [ $tap_ctl_ret == -1 ]; then
		echo "Tap-ctl execute error";
		log "Tap-ctl excute error";
		exit 1;
	fi
else
	echo "We have no $mkfs_hlfs tool";
	log "We have no $mkfs_hlfs tool";
	exit 1;
fi

#Step 5. Format block device and mount it
bd_node="/dev/xen/blktap-2/tapdev$tap_ctl_ret"
mount_dir="$sysdisk_dir/$vm_id.disk"
mkfs.ext3 $bd_node			1>/dev/null 2>&1
if [ $? != 0 ]; then
	echo "Format block device error";
	log "Format block device error";
	exit 1;
fi
mount $bd_node $mount_dir	1>/dev/null 2>&1
if [ $? != 0 ]; then
	echo "Format block device error";
	log "Format block device error";
	exit 1;
fi

#Step 6. Make system increment disk
os_type_dir="$sysdisk_dir/os_type"
sys_disk="$mount_dir/$vm_id.sys.img"
if [ -d $os_type_dir ]; then
	if [ -f $os_type_dir/$vm_os_type.img ]; then
		vhd-util snapshot -n $sys_disk -p $os_type_dir/$vm_os_type.img -m		1>/dev/null 2>&1
		if [ $? != 0 ]; then
			echo "Make system imcrement disk error";
			log "Make system imcrement disk error";
		fi
	fi
else
	echo "Error, no os type dir";
	log "Error, no os type dir";
fi

#Step 7. Make the configure file
xm_crfile=$work_dir/$vm_id.cfg
echo "vcpus = $vm_cpu_count" > $vm_crfile					1>/dev/null 2>&1
echo "name = $vm_os_type.$vm_id" >> $vm_crfile				1>/dev/null 2>&1
echo "memory = $vm_mem_size" >> $vm_crfile					1>/dev/null 2>&1
echo "vif = [ 'mac=$vm_mac_addr' ]" >> $vm_crfile			1>/dev/null 2>&1
echo "ip = $vm_ip_addr" >> $vm_crfile						1>/dev/null	2>&1
echo "netmask = 255.255.255.0" >> $vm_crfile				1>/dev/null	2>&1
echo "vncpasswd = '$vnc_passwd'" >> $vm_crfile				1>/dev/null	2>&1
echo "bootloader = "/usr/bin/pygrub"" >> $vm_crfile			1>/dev/null 2>&1
echo "disk = [ "tap2:vhd:$sys_disk,xvda,w","tap2:aio:$os_type_dir/$vm_os_type.img,xvdb,r" ]" >> $vm_crfile		1>/dev/null 2>&1

#Step 8. Create vm and get the dynamic id
xm create $xm_crfile 1>/dev/null 2>&1
sleep 15s
if [ $? != 0 ]; then
	echo "Create vm error";
	log "Create vm error";
fi
id=`xm list | grep "$vm_os_type.$vm_id" | awk '{print $2}'`

#Step 9. Check the rest stuffs
xenstore-write /local/domain/$id/key 1 					1>/dev/null 2>&1
xm block-attach $id file:/$scene_iso_file xvdf r 		1>/dev/null 2>&1
while [ 1 ]
do
	sleep 1
	N=`xenstore-read /local/domain/$id/key 1>/dev/null 2>&1`
	if [ $N==0 ]; then
		break
	else
		continue
	fi
done
xenstore-rm /local/domain/$id/key 1>/dev/null 2>&1
VBD=`xm block-list $id | grep -n '3p' | gawk '{print $1}'`		1>/dev/null 2>&1
echo "output vhd value VBD = $VBD"		1>/dev/null 2>&1
rm -rf $scene_iso_file

#Step 10. Check if start xen vm well
if [ $? -eq 0 ]; then
	echo "Start xen vm successfully"
else
	echo "Fail to start xen vm, check your stuffs :-/"
fi
exit 0;
