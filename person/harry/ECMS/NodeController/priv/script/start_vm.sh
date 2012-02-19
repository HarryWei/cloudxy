#!/bin/bash
#vm name
##Vm_id,VcpuCount,MemSize,DiskSize,Ipaddr,VncPort,VncPass,Passwd

#Vm_id=$1
name=$1
VcpuCount=$2
MemSize=$3
DiskSize=$4
Ipaddr=$5
VncPort=$6
VncPass=$7
passwd=$8

#name=`xl list | awk '{print $1,$2}' | grep $Vm_id | awk '{print $1}'`
Vm_id=`xl list | grep $name | awk '{print $2}'`
#创建iso
mkdir  /tmp/$namoe 1> /dev/null 2>&1
cd /tmp/$name 1> /dev/null 2>&1

touch  $name.txt
#echo "passwd:$passwd" > $name.txt
#echo "ip:$Ipaddr"  >> $name.txt
mkisofs -o $name.iso /tmp/$name 1> /dev/null 2>&1
cd ~
mkdir mounted$name 1> /dev/null 2>&1
#mount -o loop  /tmp/$name/$name.iso mounted$name
#ls mounted$name
#echo "============start a vm======="
xl create  /Xen/config-of-VM/$name.cfg 1> /dev/null 2>&1

#echo "start  vm  success"
sleep 15s

#echo "$name"
ID=`xl list | grep "$name" | gawk '{print $2}'`
#echo  "the domU ID is $ID"
#echo "/local/domain/$ID/iso"
xenstore-write /local/domain/$ID/iso  1 1>/dev/null 2>&1 
#1> /dev/null 2>&1

xm block-attach $ID file:/tmp/$name/$name.iso xvdf r 1>/dev/null 2>&1

while [ 1 ]
 do
  	 sleep 1
	 N=`xenstore-read /local/domain/$ID/iso 1>/dev/null 2>&1`
	 #VM configure end
	 if [ $N==0 ]
			then
              break
         else
             continue
     fi
 done
   
xenstore-rm /local/domain/$ID/iso  1> /dev/null 2>&1
#echo "xenstore rm iso success"
VBD = `xm block-list $ID | grep -n '3p' | gawk '{print $1}'` 1> /dev/null 2>&1

echo  "output vnd value VBD=  $VBD" 1> /dev/null 2>&1
#echo " `xm block-list $ID | sed -n '/\/$ID\//p' | sed '1d' | gawk '{print $1}'` "
# vbd=` xm block-list 52 | grep '\/52\/' | sed '1d' | awk '{ print $1}'` 1> /dev/null 2>&1
xm block-detach $ID $VBD 1> /dev/null 2>&1

rm -rf /tmp/$name.iso

if [ $? -eq 0 ]
then
	echo "SUCC"
else 
	echo "FAIL"
fi
