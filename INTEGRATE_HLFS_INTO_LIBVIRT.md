# 简介 #
### Libvirt 存储管理分为两种模式 ###
  1. 在线模式 -- 应用作为一个服务在运行，例如模拟的块设备等等。
  1. 离线模式 -- 即standalone方式运行，应用执行完就退出。

### 在线模式 ###
  * 通过Libvirt创建虚拟机和HLFS格式的磁盘（目前HLFS支持QEMU和XEN）。
  * 通过Libvirt和虚拟机交互，进而通过虚拟机对HLFS格式磁盘进行一些必要的操作。

### 离线模式 ###
  * 通过Libvirt创建HLFS格式的Pool。
  * 通过Libvirt在Pool上创建HLFS格式的Volumes。
  * 通过Libvirt对HLFS Pool和HLFS Volumes进行一些必要的操作。

# 在线模式测试 #
### 测试环境搭建 ###
  * 搭建HLFS环境，详见http://code.google.com/p/cloudxy/wiki/HlfsUserManual
  * 搭建QEMU环境，详见http://code.google.com/p/cloudxy/wiki/HLFS_SUPPORT_QEMU
  * 搭建Libvirt环境
```
  1. git clone git://libvirt.org/libvirt.git；cd libvirt
  2. git reset --hard v1.0.1
  3. wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_network_disk.patch
  4. wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_add_classpath.patch
  5. git apply hlfs_driver_for_libvirt_network_disk.patch
  6. git apply hlfs_driver_for_libvirt_add_classpath.patch
  7. ./autogen.sh
  8. ./configure
  9. make
  10. sudo make install
注意：a, HLFS driver for Libvirt network disk 是针对Libvirt v1.0.1
     b, 我的测试系统是ubuntu 10.04, 安装过程中如果本地软件库安装不上依赖库，或者本地软件库安装依赖库版本过低，那么可以下载对应依赖库源码进行编译安装。
```
  * 搭建环境过程中可能需要安装一些依赖库，缺什么库文件，就安装对应库文件 —— 我有很多就是从源码包安装的。
### 从HLFS Volumes启动虚拟机 ###
  1. wget http://wiki.qemu.org/download/linux-0.2.img.bz2
  1. bunzip2 linux-0.2.img.bz2
  1. mkdir /tmp/testenv
  1. qemu-img convert -t directsync linux-0.2.img hlfs:local:///tmp/testenv/testfs
  1. cat > hlfs.xml
```
<domain type='qemu'>
<name>testvm</name>
<memory>1048576</memory>
<os>
<type arch='x86_64'>hvm</type>
</os>
<devices>
<disk type='network'>
<source protocol="hlfs" name="local:///tmp/testenv/testfs"/>
<target dev='hda' bus='ide'/>
</disk>
<graphics type='vnc' port='-1' autoport='yes'/>
</devices>
</domain> 
```
  1. virsh create hlfs.xml
  1. vncviewer localhost
```
注意：没有vncviewer的话，sudo apt-get install tightvnc-java(debian是这样，其他distro可以找对应软件安装)去安装一个小客户端吧，登陆时密码为空！
```
### 从HLFS 快照启动虚拟机 ###
  1. wget http://wiki.qemu.org/download/linux-0.2.img.bz2
  1. bunzip2 linux-0.2.img.bz2
  1. mkdir /tmp/testenv
  1. qemu-img convert -t directsync linux-0.2.img hlfs:local:///tmp/testenv/testfs
  1. qemu-img snapshot -c snapshot1 hlfs:local:///tmp/testenv/testfs
  1. cat > hlfs.xml
```
<domain type='qemu'>
<name>testvm</name>
<memory>1048576</memory>
<os>
<type arch='x86_64'>hvm</type>
</os>
<devices>
<disk type='network'>
<source protocol="hlfs" name="local:///tmp/testenv/testfs%snapshot1"/>
<target dev='hda' bus='ide'/>
</disk>
<graphics type='vnc' port='-1' autoport='yes'/>
</devices>
</domain> 
```
  1. virsh create hlfs.xml
  1. vncviewer localhost
```
注意：没有vncviewer的话，sudo apt-get install tightvnc-java(debian是这样，其他distro可以找对应软件安装)去安装一个小客户端吧，登陆时密码为空！
```
# 离线模式测试 #
### 测试环境搭建 ###
这个过程与在线模式的测试环境搭建相同，需要注意的是，离线模式在搭建libvirt环境时，需要下载如下补丁并应用：
```
wget http://cloudxy.googlecode.com/svn/branches/hlfs/person/harry/hlfs/patches/hlfs_driver_for_libvirt_offline_storage.patch
git apply hlfs_driver_for_libvirt_offline_storage.patch

注意：测试离线模式之前需要在/etc/profile里面加入环境变量HLFS_INSTALL_PATH 并给 PATH添加HLFS可执行文件路径，具体如下。
export HLFS_INSTALL_PATH=/usr/local/hlfs  (对应自己的HLFS安装目录)
export PATH=$PATH:/usr/local/hlfs/bin/  (对应自己的HLFS可执行文件路径)
source /etc/profile  (使环境变量生效)
```

### HLFS Pool操作 ###
  1. 启动libvirt服务端进程，libvirt/daemon/libvirtd
  1. 执行客户端程序，创建HLFS Pool, virsh pool-create pool\_hlfs.xml
```
$ cat pool_hlfs.xml 
<pool type="hlfs">
	<name>local:///tmp/testenv</name>
	<source>
	<name>local:///tmp/testenv</name>
	<host name='localhost' port='7000'/>
	</source>
</pool>
```
  1. 删除HLFS Pool, virsh pool-destroy local:///tmp/testenv
```
注意：
a, pool_hlfs.xml中的name域有两种模式，local和hdfs，也即HLFS的两种模式，路径是可配置的。
b, pool_hlfs.xml的两个name域内容需要一致。
c, 删除HLFS Pool 时，只需给出对应的HLFS Pool name。
```

### HLFS Volume操作 ###
  1. 创建HLFS Volume, virsh vol-create local:///tmp/testenv hlfs\_volume.xml
```
$ cat hlfs_volume.xml
<volume>
<name>testfs</name>
<key>hlfs/testfs</key>
<source>
</source>
<capacity unit='bytes'>1000000000</capacity>
<allocation unit='bytes'>1000000000</allocation>
<target>
<path>hlfs:local:///tmp/testenv/testfs</path>
<format type='unknown'/>
<permissions>
<mode>00</mode>
<owner>0</owner>
<group>0</group>
</permissions>
</target>
</volume>
```
  1. 删除HLFS Volume, virsh vol-delete testfs
```
注意：
a, 创建HLFS Volume时需要指定之前创建的HLFS Pool name。
b, 删除HLFS Volume时只需给出HLFS Volume name。
```

# 注意事项 #
  * 目前HLFS支持Libvirt storage并非全部功能，因为有些功能Libvirt还没提供接口。
  * 搭建环境过程中，可能会遇到这样那样的问题，如果实在无法解决们可以发邮件到我们群邮件(cloudxy@googlegroups.com)寻求帮助。
  * 由于本人水平有限，如果你在实验的过程中发现HLFS driver for Libvirt有问题，或者某些步骤不是很合理，存在问题，请及时指正。

# 感谢 #
在完成HLFS driver for Libvirt的过程中得到了很多人的帮助，他们孜孜不倦的教诲令我深受感动，这里贴出他们的名字以示我的谢意，同时提醒我随时像他们学习多多帮助别人。
```
陈莉君老师：激励我不断探索，不断进步，思想上给予很大的帮助。
康华师哥：从头到尾，任何问题都给予帮助，学到了很多。
Eric Blake: 帮助我理解了Libvirt中 storage 的管理方式，以及其他与Libvirt的问题。
MORITA Kazutaka: 对于Sheepdog的任何问题，他都孜孜不倦的给予解答和帮助。
张典博师哥：完成HLFS driver for Libvirt 之后，一起测试，提出了一些建议和需要完善的地方。
还有一些这里没有提到的人，也给予了很大的帮助，感谢他们。
```

---

Written by: Harry Wei (harryxiyou@gmail.com)
<br>
Reviewed by: 张典博 (littlesmartsmart@gmail.com)