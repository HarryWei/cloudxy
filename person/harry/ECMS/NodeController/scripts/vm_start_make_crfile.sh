#!/bin/bash

#Step 7
vm_crfile=$work_dir/$vm_id.cfg

#Step 7. Make the configure file
	echo "vcpus = $vm_cpu_count" > $vm_crfile				1>/dev/null 2>&1
	echo "name = $vm_os_type.$vm_id" >> $vm_crfile				1>/dev/null 2>&1
	echo "memory = $vm_mem_size" >> $vm_crfile				1>/dev/null 2>&1
	echo "vif = [ 'mac=$vm_mac_addr' ]" >> $vm_crfile			1>/dev/null 2>&1
	echo "ip = $vm_ip_addr" >> $vm_crfile					1>/dev/null 2>&1
	echo "netmask = 255.255.255.0" >> $vm_crfile				1>/dev/null 2>&1
	echo "vncpasswd = '$vnc_passwd'" >> $vm_crfile				1>/dev/null 2>&1
	echo "bootloader = "/usr/bin/pygrub"" >> $vm_crfile			1>/dev/null 2>&1
	echo "disk = [ "tap2:vhd:$sys_disk,xvda,rw" ]" >> $vm_crfile		1>/dev/null 2>&1
