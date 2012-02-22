#!/bin/bash

#Step 6
os_type_dir="$sysdisk_dir/os_type"
sys_disk="$mount_dir/$vm_id.sys.img"

#Step 6. Make system increment disk
if [ -d $os_type_dir ]; then
	if [ -f $os_type_dir/$vm_os_type.img ]; then
		vhd-util snapshot -n $sys_disk -p $os_type_dir/$vm_os_type.img -m		1>/dev/null 2>&1
		if [ $? != 0 ]; then
			echo "Make system imcrement disk error";
			log "Make system imcrement disk error";
			exit 1;
		fi
	fi
else
	echo "Error, no os type dir";
	log "Error, no os type dir";
	exit 1;
fi
