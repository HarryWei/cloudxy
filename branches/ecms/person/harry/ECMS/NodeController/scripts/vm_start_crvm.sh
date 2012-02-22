#!/bin/bash

#Step 8. Create vm and get the dynamic id
	xm create $xm_crfile 1>/dev/null 2>&1
	sleep 15s
	if [ $? != 0 ]; then
		echo "Create vm error";
		log "Create vm error";
		exit 1;
	fi
	id=`xm list | grep "$vm_os_type.$vm_id" | awk '{print $2}'`
