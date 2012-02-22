#!/bin/bash

#Step 4
tools_dir="$HOME/tools"
mkfs_hlfs="$tools_dir/mkfs.hlfs"

#Step 4. Create system disk. TODO support *hdfs* pattern
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
			exit 1;
		else
			echo "fail to remove $sysdisk_dir/$vm_id, check permission";
			log "fail to remove $sysdisk_dir/$vm_id, check permission";
			exit 1;
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
