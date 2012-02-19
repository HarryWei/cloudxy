#!/bin/bash
#argument: vmid

# This script is used for shutdown a VM

name=$1

VM_shutdown()
{
#		echo -e "\nOK..Let's shutdown this VM \n"
		sleep 2s
		xm shutdown $name >> /dev/null 2>/dev/null
		
		if [ $? -eq 0 ] ; then
			echo  "SUCC"
		else
			echo  "FAIL"
		fi
}

VM_shutdown

