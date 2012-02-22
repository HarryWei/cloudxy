#!/bin/bash

sysdisk_dir="$HOME/sysdisk"
sysname="$vm_id.sys.img"

#Step 2. Check if first start (Judge if sys_disk exist).
#If so, do step 3 in sequnce. If not, jump to step 7.
#Hlfs local URI pattern "local:///$HOME/sysdisk"
#Hlfs hdfs URI pattern "hdfs:///$HOME/sysdisk"
if [ -f $sysdisk_dir/$sysname ]; then 
	echo "$sysname exists, jump to Step 7";
	log "$sysname exists, jump to Step 7";
	rest;
fi
