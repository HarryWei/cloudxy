#!/bin/bash

bd_node="/dev/xen/blktap-2/tapdev$tap_ctl_ret"
mount_dir="$sysdisk_dir/$vm_id.disk"


#Step 5. Format block device and mount it
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
