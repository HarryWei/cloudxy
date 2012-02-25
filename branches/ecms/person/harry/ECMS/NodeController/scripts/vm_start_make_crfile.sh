#!/bin/bash

echo "This is Step 7, make start configure file, enter vm_start_make_crfile.sh file."
#Step 7
vm_id=$1;
vm_cpu_count=$2;
vm_mem_size=$3;
vm_ip_addr=$4;
vm_mac_addr=$5;
vnc_port=$6;
vnc_passwd=$7;
vm_os_type=$8;
work_dir=$HOME/$vm_id
vm_crfile=$work_dir/$vm_id.cfg

#Step 7. Make the configure file
echo "vcpus = $vm_cpu_count" > $vm_crfile
echo "name = "$vm_os_type.$vm_id"" >> $vm_crfile
echo "memory = $vm_mem_size" >> $vm_crfile		
echo "vif = [ 'mac=$vm_mac_addr' ]" >> $vm_crfile
echo "ip = $vm_ip_addr" >> $vm_crfile			
echo "netmask = 255.255.255.0" >> $vm_crfile			
echo "vncpasswd = '$vnc_passwd'" >> $vm_crfile			
echo "bootloader = "/usr/bin/pygrub"" >> $vm_crfile		
echo "disk = [ 'tap2:vhd:$sys_disk,xvda,rw' ]" >> $vm_crfile	

echo "leave vm_start_make_crfile.sh file.---------------->"
