#!/bin/bash

echo "This is Step 6, create increment disk.enter vm_start_cr_sys_incre_disk.sh file."
#Step 6
vm_id=$1
vm_os_type=$2
sysdisk_dir=$HOME/sysdisk
os_type_dir="$HOME/os_type"
sys_disk="$sysdisk_dir/$vm_id.sys.img"

#Step 6. Make system increment disk
if [ -d $os_type_dir ]; then
	if [ -f $os_type_dir/$vm_os_type.img ]; then
		vhd-util snapshot -n $sys_disk -p $os_type_dir/$vm_os_type.img -m		1>/dev/null 2>&1
		if [ $? == -1 ]; then
			echo "Make system imcrement disk error";
			log "Make system imcrement disk error";
			exit 1;
		fi
	else
		echo "No this kind of os type";
		source ./log.sh "No this kind of os type";
	fi
else
	echo "Error, no os type dir";
	log "Error, no os type dir";
	exit 1;
fi
echo "leave vm_start_cr_sys_incre_disk.sh file--------------------->"
