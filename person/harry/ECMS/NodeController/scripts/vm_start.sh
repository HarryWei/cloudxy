#!/bin/bash


#File name: vm_start.sh
#Author: Harry Wei <harryxiyou@gmail.com> 2012 2 19
#Program descripton: This script starts a xen virtual machine
#Change Log: 2012-2-19 harry Created this script

#!/bin/bash

#Get the vm_start Parameters
if [ $# != 20 ]; then
	echo "Parameters Error";
	source ./log.sh "Parameters Error";
	source ./vm_start_usage.sh;
	exit 1;
fi

if [ "$1" != "-i" ] || [ "$3" != "-c" ] ||
	[ "$5" != "-s" ] || [ "$7" != "-p" ] ||
	[ "$9" != "-h" ] || [ "${11}" != "-a" ] ||
	[ "${13}" != "-m" ] || [ "${15}" != "-o" ] ||
	[ "${17}" != "-w" ] || [ "${19}" != "-t" ]; then
	echo "Parameters error";
	source ./log.sh "Parameters error";
	source ./vm_start_usage.sh;
	exit 1;
fi

while getopts i:c:s:p:h:a:m:o:w:t: option
do
	case "$option" in
		i) vm_id=$OPTARG;;
		c) vm_cpu_count=$OPTARG;;
		s) vm_mem_size=$OPTARG;;
		p) vm_passwd=$OPTARG;;
		h) vm_hostname=$OPTARG;;
		a) vm_ip_addr=$OPTARG;;
		m) vm_mac_addr=$OPTARG;;
		o) vnc_port=$OPTARG;;
		w) vnc_passwd=$OPTARG;;
		t) vm_os_type=$OPTARG;;
		?) source ./vm_start_usage.sh; source ./log.sh "Parameters Error"; exit 1;;
	esac
done

echo "vm_id is $vm_id"
echo "vm_cpu_count is $vm_cpu_count"
echo "vm_mem_size is $vm_mem_size"
echo "vm_passwd is $vm_passwd"
echo "vm_hostname is $vm_hostname"
echo "vm_ip_addr is $vm_ip_addr"
echo "vm_mac_addr is $vm_mac_addr"
echo "vnc_port is $vnc_port"
echo "vnc_passwd is $vnc_passwd"
echo "vm_os_type is $vm_os_type"
