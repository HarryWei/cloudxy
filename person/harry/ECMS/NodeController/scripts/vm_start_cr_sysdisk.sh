#!/bin/bash

vm_id=$1
#Step 4
tools_dir="$HOME/tools"
mkfs_hlfs="$tools_dir/mkfs.hlfs"
sysdisk_dir="$HOME/sysdisk"

#Step 4. Create system disk. TODO support *hdfs* pattern
if [ -d $sysdisk_dir ]; then
		echo "$sysdisk_dir exist"
		source ./log.sh "log dir has existed!"
else
	mkdir $sysdisk_dir				1>/dev/null 2>&1
fi
if [ -f $mkfs_hlfs ]; then
	if [ -d $sysdisk_dir/$vm_id ]; then
		if rm -rf $sysdisk_dir/$vm_id		1>/dev/null 2>&1
		then
			echo "remove $sysdisk_dir/$vm_id successfully";
			source ./log.sh "remove $sysdisk_dir/$vm_id successfully";
		else
			echo "fail to remove $sysdisk_dir/$vm_id, check permission";
			source ./log.sh "fail to remove $sysdisk_dir/$vm_id, check permission";
			exit 1;
		fi	
	else
		mkdir $sysdisk_dir/$vm_id;
	fi
	$mkfs_hlfs -u local://$sysdisk_dir/$vm_id -b 8192 -s 67108864 -m 1024
	if [ $? == -1 ]; then
		echo "Format hlfs error";
		source ./log.sh "Format hlfs error";
		exit 1;
	fi
	tap-ctl create -a hlfs:local://$sysdisk_dir/$vm_id		1>/dev/null 2>&1
	tap_ctl_ret=$?
	if [ $tap_ctl_ret == -1 ]; then
		echo "Tap-ctl execute error";
		source ./log.sh "Tap-ctl excute error";
		exit 1;
	fi
	echo "tap-ctl device number is $tap_ctl_ret";
	source ./log.sh "tap-ctl device number is $tap_ctl_ret";
else
	echo "We have no $mkfs_hlfs tool";
	source ./log.sh "We have no $mkfs_hlfs tool";
	exit 1;
fi
