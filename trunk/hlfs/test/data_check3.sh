#!/bin/bash
#For Brother Kang
data_check(){
DATA_SIZE=$1
echo "data check size is %DATA_SIZE"
echo "1 setup local test env"
umount /dev/nbd1
rm /tmp/testenv -rf >/dev/null 2>&1
mkdir /tmp/testenv  
../../../hlfs/output/bin/mkfs.hlfs -l local:///tmp/testenv -f testfs -b 8192
[ $? -ne 0 ] && exit -1
echo "2 setup nbd block device"
modprobe nbd >/dev/null 2>&1
../../../hlfs/output/bin/nbd-client -d /dev/nbd1 >/dev/null 2>&1
killall -9 nbd-server >/dev/null 2>&1
rm /tmp/nbd-* -rf >/dev/null 2>&1
../../../hlfs/output/bin/nbd-server 20000 local:///tmp/testenv &
sleep 5
../../../hlfs/output/bin/nbd-client bs=512 127.0.0.1 20000 testfs /dev/nbd1 40
[ $? -ne 0 ] && exit -1
mkfs.ext3 /dev/nbd1

../../../hlfs/output/bin/nbd-client -d /dev/nbd1 >/dev/null 2>&1
killall -9 nbd-server >/dev/null 2>&1

return 0
#sync 
#sleep 1
#fsck /dev/nbd1
#rm -rf tmpmnt >/dev/null 2>&1
mkdir -p tmpmnt >/dev/null 2>&1
mount /dev/nbd1 tmpmnt
echo "3 import DATAFILE into nbd1"
rm DATA.exp >/dev/null 2>&1
rm DATA.tmp >/dev/null 2>&1
rm DATA.imp >/dev/null 2>&1
dd if=DATA of=DATA.tmp bs=16K count=${DATA_SIZE}K 
dd if=DATA.tmp of=DATA.imp bs=8K count=${DATA_SIZE}K 
#dd if=DATA of=DATA.imp bs=1M count=9
dd if=DATA.imp of=tmpmnt/xxx bs=16K count=${DATA_SIZE}K oflag=direct,nonblock
#dd if=DATA.imp of=tmpmnt/xxx oflag=direct,nonblock
#cp DATA.imp tmpmnt/xxx
#syncy
#echo "3">/proc/sys/vm/drop_caches 
ls tmpmnt/xxx -lah
IMPORTMD5=`md5sum DATA.imp|awk '{print$1}'` 
dd if=tmpmnt/xxx of=DATA.exp bs=8K count=${DATA_SIZE}K iflag=direct,nonblock
#cp tmpmnt/xxx DATA.exp
EXPORTMD5=`md5sum DATA.exp|awk '{print$1}'`
echo "---------------------------------result--------------------------------"
echo "import file md5 $IMPORTMD5"
echo "export file md5 $EXPORTMD5"

if [ $IMPORTMD5 == $EXPORTMD5 ]
   then 
     echo "$DATA_SIZE*8 M size data check pass" 
     umount /dev/nbd1
     ../../../hlfs/output/bin/nbd-client -d /dev/nbd1 >/dev/null 2>&1
     killall -9 nbd-server >/dev/null 2>&1
   else
     echo "$DATA_SIZE*8 M size data check failed"
     umount /dev/nbd1
     ../../../hlfs/output/bin/nbd-client -d /dev/nbd1 >/dev/null 2>&1
     killall -9 nbd-server >/dev/null 2>&1
     exit 
fi
return 0;
}


TEST_MUCH=$1
echo "data check much : $TEST_MUCH"
i=1
while [ $i -le $TEST_MUCH ]
do
       data_size=$[$i ** 2]
       echo $data_size
       data_check $data_size
       let i=$i+1
done
    

