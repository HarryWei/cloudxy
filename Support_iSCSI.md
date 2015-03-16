# Introduction #

Support iSCSI protocol by patching TGT. This wiki page is intended to outline
some steps need to quickly get the TGT working with HLFS.

# Installation #

Start by installing TGT:http://stgt.sourceforge.net/.

  * Download the source code of TGT.
```
git clone git://github.com/fujita/tgt.git
```
  * Apply the patch located in the project cloudxy.
```
$cd tgt
$git reset --hard v1.0.9
$git apply HLFS_HOME/patches/hlfs_driver_for_tgt.patch
```
  * configure your Makefile.
```
$vim usr/Makefile
# line 2: Set the hlfs_home to your own.
# line 31, line 32: Set the glib path to your own.
# line 35, line 36: Set the JAVA path to your own.
```
  * Copy the libhlfs.so to /usr/lib directory.
```
$sudo cp HLFS_HOME/output/lib32/libhlfs.so /usr/lib
```
  * Compile and Installation
```
$make 
$sudo make install
```

# Demo #

  * Format HLFS.
```
$cd HLFS_HOME
$./auto_local.sh
```
  * Start tgt deamon.
```
$sudo service tgt restart
```
  * Create an iSCSI target
```
sudo tgtadm --lld iscsi --mode target --op new --tid 1 --targetname iqn.2012-11.com.example:hlfs
```
  * Create an iSCSI LUN
```
sudo tgtadm --lld iscsi --mode logicalunit --op new --tid 1 --lun 1 -b local:///tmp/testenv/testfs --bstype hlfs
```
  * Show the result
```
sudo tgtadm --lld iscsi --mode target --op show
```
  * Add IP wildcard to allow all initiators
```
sudo tgtadm --lld iscsi --mode target --op bind --tid 1 -I ALL
```
  * Install open-iscsi:
```
sudo apt-get install open-iscsi
```
  * Discovery target:
```
sudo iscsiadm -m discovery -t st -p 202.117.132.221:3260
```
  * Login target:
```
sudo iscsiadm -m node --targetname iqn.2012-11.com.example:hlfs --portal 202.117.132.221 --login
```
  * Do something like using a local block device:
```
ls /dev/sdb
```
---

Wang Sen <kelvin.xupt@gmail.com>
2012.11.18