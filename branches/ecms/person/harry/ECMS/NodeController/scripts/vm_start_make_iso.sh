#!/bin/bash

scene_file="/tmp/initialize"
scene_iso_file="$HOME/$vm_id/$vm_id.iso"


#Step 3. Make iso image for vm_passwd and vm_hostname
echo "hostname: $vm_hostname" > $scene_file		1>/dev/null 2>&1
echo "password: $vm_passwd" >> $scene_file		1>/dev/null 2>&1
mkisofs -o $scene_iso_file $scene_file			1>/dev/null	2>&1
if [ $? != 0 ]; then
	echo "mkisofs error";
	log "mkisofs error";
	exit 1;
fi
