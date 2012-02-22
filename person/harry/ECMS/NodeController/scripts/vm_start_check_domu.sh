#!/bin/bash


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
	echo "output vhd value VBD = $VBD"								1>/dev/null 2>&1
	rm -rf $scene_iso_file

#Step 10. Check if start xen vm well
	if [ $? -eq 0 ]; then
		echo "Start xen vm successfully"
		log "Start xen vm successfully"
	else
		echo "Fail to start xen vm, check your stuffs :-/"
		log "Fail to start xen vm, check your stuffs :-/"
		exit 1;
	fi
	exit 0;
