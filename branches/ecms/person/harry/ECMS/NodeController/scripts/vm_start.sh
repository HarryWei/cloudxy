#!/bin/bash


#File name: vm_start.sh
#Author: Harry Wei <harryxiyou@gmail.com> 2012 2 19
#Program descripton: This script starts a xen virtual machine
#Change Log: 2012-2-19 harry Created this script

FILEPATH="$PWD/vm_start.sh"
LOGFILE=/tmp/vm_ops.log
error_log() {
	echo "[`date "+%Y/%m/%d %H:%M:%S"` $FILEPATH] "$*"" >> $LOGFILE
}
#This function just prints the script's usage
usage() {
	echo "Usage: $0 -i <vm_id> -c <vm_cpu_count> -s <vm_mem_size> -p <vm_passwd> -h <vm_hostname> -a <vm_ip_addr> -m <vm_mac_addr> -o <vnc_port> -w <vnc_passwd> -t <vm_os_type>"
}

debug() {
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
}

if [ $# != 20 ]; then
	usage;
	error_log "Parameters Error";
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
		?) usage; error_log "Parameters Error"; exit 1;;
	esac
done

debug;
exit 0;
