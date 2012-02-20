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
		if rm -rf $work_dir
		then 
			echo "Remove dir successfully";
			cd $HOME;
			mkdir $vm_id;
			cd $vm_id;
		else
			echo "Fail to remove dir, check your dir's permission!";
			log "Fail to remove dir, check your dir's permission!";
		fi
else
	mkdir $work_dir;
	cd $work_dir;
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
echo "hostname: $vm_hostname" > $scene_file
echo "password: $vm_passwd" >> $scene_file
mkisofs -o $scene_iso_file $scene_file

#Step 4. Create system disk. TODO support *hdfs* pattern
tools_dir="$HOME/tools"
mkfs_hlfs="$tools_dir/mkfs.hlfs"
if [ -d $sysdisk_dir ]; then
		echo "$sysdisk_dir exist"
else
	mkdir $sysdisk_dir
fi
if [ -f $mkfs_hlfs ]; then
	if [ -d $sysdisk_dir/$vm_id ]; then
		if rm -rf $sysdisk_dir/$vm_id
		then
			echo "remove $sysdisk_dir/$vm_id successfully";
			log "remove $sysdisk_dir/$vm_id successfully";
		else
			echo "fail to remove $sysdisk_dir/$vm_id, check permission";
			log "fail to remove $sysdisk_dir/$vm_id, check permission";
		fi	
	fi
	$mkfs_hlfs -u local:///$sysdisk_dir/$vm_id -b 8192 -s 67108864 -m 1024
	tap-ctl create -a hlfs:local:///$sysdisk_dir/$vm_id
	tap_ctl_ret=$?
	if [ $tap_ctl_ret != 0 ]; then
		echo "Tap-ctl execute error";
		log "Tap-ctl excute error";
		exit 1;
	fi
else
	echo "We have no $mkfs_hlfs tool";
	log "We have no $mkfs_hlfs tool";
	exit 1;
fi

exit 0;
