#!/bin/bash
#It is just for Harry Wei's directory constructure
#So if you want to use it, please change into yours.
#2011 10 27 in Xiyou

data_check(){
	DATA_SIZE=$1
	echo "data check size is $DATA_SIZE"
	echo "1 setup local test env"
	umount /dev/nbd3
	rm /tmp/testenv -rf >/dev/null 2>&1
	rm /tmp/nbd -rf >/dev/null 2>&1
	mkdir /tmp/testenv
	mkdir /tmp/nbd
	/home/jiawei/workshop9/hlfs1/output/bin/mkfs.hlfs -l local:///tmp/testenv -f testfs -b 8192 -s 67108864
	[ $? -ne 0 ] && exit -1
	sleep 2 
	echo "2 setup nbd block device"
	modprobe nbd >/dev/null 2>&1
	/home/jiawei/workshop9/hlfs1/nbd-2.9.15/output/bin/nbd-client -d /dev/nbd3 >/dev/null 2>&1
	killall -9 nbd-server >/dev/null 2>&1
	rm /tmp/nbd-* -rf >/dev/null 2>&1
	/home/jiawei/workshop9/hlfs1/nbd-2.9.15/output/bin/nbd-server 20000 local:///tmp/testenv &
	sleep 5
	/home/jiawei/workshop9/hlfs1/nbd-2.9.15/output/bin/nbd-client bs=512 127.0.0.1 20000 testfs /dev/nbd3 1000
	[ $? -ne 0 ] && exit -1
	sleep 2
	mkfs /dev/nbd3 > /dev/null 2>&1
	sleep 5
	mount /dev/nbd3 /tmp/nbd > /dev/null 2>&1
	echo "3 import DATAFILE into nbd*"
	rm DATA.exp >/dev/null 2>&1
	rm DATA.imp >/dev/null 2>&1
	dd if=DATA of=DATA.imp bs=8K count=${DATA_SIZE}K
	dd if=DATA.imp of=/tmp/nbd/nbd bs=8K count=${DATA_SIZE}K oflag=direct,nonblock
	IMPORTMD5=`md5sum DATA.imp|awk '{print$1}'`
	sleep 2
	dd if=/tmp/nbd/nbd of=DATA.exp bs=8k count=${DATA_SIZE}k iflag=direct,nonblock
	EXPORTMD5=`md5sum DATA.exp|awk '{print$1}'`
	echo "---------------------------------result--------------------------------"
	echo "import file md5 $IMPORTMD5"
	echo "export file md5 $EXPORTMD5"
	if [ $IMPORTMD5 == $EXPORTMD5 ]   
		then
			echo "$DATA_SIZE*8 M size data check pass" 
			umount /dev/nbd3
			/home/jiawei/workshop9/hlfs1/nbd-2.9.15/output/bin/nbd-client -d /dev/nbd3 >/dev/null 2>&1
			killall -9 nbd-server >/dev/null 2>&1
	else
		echo "$DATA_SIZE*8 M size data check failed"
			umount /dev/nbd3
			/home/jiawei/workshop9/hlfs1/nbd-2.9.15/output/bin/nbd-client -d /dev/nbd3 >/dev/null 2>&1
			killall -9 nbd-server >/dev/null 2>&1    
			exit
		fi
	return 0;
}

TEST_MUCH=$1
echo "data check much : $TEST_MUCH"
i=1;
while [ $i -le $TEST_MUCH ]
do
	data_size=$[$i ** 2]
	echo $data_size
	data_check $data_size
	let i=$i+1
done

