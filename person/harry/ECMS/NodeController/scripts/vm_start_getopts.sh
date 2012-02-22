#!/bin/bash

#Get the Parameters
if [ $# != 20 ]; then
	echo "Parameters Error";
	log "Parameters Error";
	usage;
	exit 1;
fi

if [ "$1" != "-i" ] || [ "$3" != "-c" ] ||
	[ "$5" != "-s" ] || [ "$7" != "-p" ] ||
	[ "$9" != "-h" ] || [ "${11}" != "-a" ] ||
	[ "$13" != "-m" ] || [ "${15}" != "-o" ] ||
	[ "$17" != "-w" ] || [ "${19}" != "-t" ]; then
	echo "Parameters error";
	log "Parameters error";
	usage;
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
		?) usage; log "Parameters Error"; exit 1;;
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
