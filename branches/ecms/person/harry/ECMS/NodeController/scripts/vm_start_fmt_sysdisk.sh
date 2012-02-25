#!/bin/bash

echo "This is Step 5, format sysdisk.enter vm_start_fmt_sysdisk.sh file."
tap_num=$1;
vm_id=$2;
bd_node="/dev/xen/blktap-2/tapdev$tap_num"
sysdisk_dir=$HOME/sysdisk;
mount_dir="$sysdisk_dir/$vm_id.disk"


#Step 5. Format block device and mount it
mkfs.ext3 $bd_node			1>/dev/null 2>&1
if [ $? == -1 ]; then
	echo "Format block device error";
	log "Format block device error";
	exit 1;
fi
if [ -d $mount_dir ]; then
	echo "mount dir exists, we need not mkdir it";
	./log.sh "mount dir exists, we need not mkdir it";
else
	mkdir $mount_dir;
fi
mount $bd_node $mount_dir	1>/dev/null 2>&1
if [ $? == -1 ]; then
	echo "Format block device error";
	log "Format block device error";
	exit 1;
fi
echo "leave vm_start_fmt_sysdisk.sh file----------->"
