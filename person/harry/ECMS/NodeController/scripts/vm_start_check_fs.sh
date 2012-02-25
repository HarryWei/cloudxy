#!/bin/bash

echo "This is Step 2, check if first start.enter vm_start_check_fs.sh file."
vm_id=$1
sysdisk_dir="$HOME/sysdisk"
sysname="$vm_id.sys.img"

#Step 2. Check if first start (Judge if sys_disk exist).
#If so, do step 3 in sequnce. If not, jump to step 7.
#Hlfs local URI pattern "local:///$HOME/sysdisk"
#Hlfs hdfs URI pattern "hdfs:///$HOME/sysdisk"
if [ -f $sysdisk_dir/$sysname ]; then 
	echo "$sysname exists, jump to Step 7";
	./log.sh "$sysname exists, jump to Step 7";
#TODO add junp to Step 7 stuffs
else
	echo "This is first start vm";
	./log.sh "This is first start vm";
fi
echo "leave vm_start_check_fs.sh file--------------------->"
