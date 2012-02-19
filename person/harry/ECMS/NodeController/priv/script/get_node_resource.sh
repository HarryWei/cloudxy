#!/bin/bash
# you should extrace these info form your system
#echo "node2@127.0.0.1 192.168.11.129 2 1000 1000 500 500"
#hostname=`hostname`
hostname="node2@127.0.0.1"
ip=`ifconfig eth0|grep inet|sed -n '1p'|awk '{print $2}'|cut -c6-21`
tmem=`(xm info|grep total_memory|awk '{print $3}') 2>/dev/null`
fmem=`(xm info|grep free_memory|awk '{print $3}') 2>/dev/null`
cpu=`(xm info|grep nr_cpus|awk '{print $3}') 2>/dev/null`
thd=`df|grep sda|awk -v sum=0 '{sum+=$2}END{print sum}'`
fhd=`df|grep sda|awk -v sum=0 '{sum+=$4}END{print sum}'`
echo "$hostname $ip $tmem $fmem $cpu $thd $fhd"
