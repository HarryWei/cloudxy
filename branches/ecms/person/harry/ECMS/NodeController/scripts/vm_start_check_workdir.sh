#!/bin/bash

vm_id=$1
work_dir="$HOME/$vm_id"

#Step 1. Check if the dir exists. if so, delete it and 
#recreate it. if not, create it directly.
if [ -d $work_dir ]; then
		if rm -rf $work_dir		1>/dev/null 2>&1
		then 
			echo "Remove dir successfully";
			mkdir $work_dir;	1>/dev/null 2>&1
		else
			echo "Fail to remove dir, check your dir's permission!";
			./log.sh "Fail to remove dir, check your dir's permission!";
			exit 1;
		fi
else
	mkdir $work_dir;			1>/dev/null 2>&1
fi
