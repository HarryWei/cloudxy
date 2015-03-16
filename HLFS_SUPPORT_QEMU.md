# 简介 #
我们已经将HLFS移植到QEMU环境下，从而支持在KVM等虚拟环境下使用其。


# 下载和编译 #
  1. 下载 qemu - git clone git://git.qemu.org/qemu.git
```
注意：HLFS driver patch for QEMU V1.3.0, clone完mailine的git tree,还需执行下面命令，回滚到 QEMU V1.3.0.
      git reset --hard v1.3.0
```
  1. 下载patch for qemu - http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch
  1. patch it - 在qemu目录中执行 ：git apply hlfs\_driver\_for\_qemu.patch
  1. 在qemu目录下的configure文件中修改如下内容
```
GLIB_DIR1_INC=/usr/lib/glib-2.0/include
GLIB_DIR2_INC=/usr/include/glib-2.0
HLFS_DIR=/home/kanghua/workshop3.bak/hlfs
JVM_DIR=/usr/lib/jvm/java-6-sun/

注意：以上几个变量所指的位置为你系统环境的实际地址
```
  1. 编译安装QEMU
```
$./configure --enable-hlfs --with-coroutine=gthread 
这里必须特别说明一下 coroutine是qemu中提高并发效率新机制（默认coroutine使用的backend是ucontext），但可惜因为我们使用libhdfs，其使用jni创建jvm. 调试中发现ucontext机制的stack管理和jvm的stack似乎有冲突，具体原因未明（如果那位有识之士帮忙调查清楚，则感激不尽）。所以qemu的coroutine请配置为传统的thread模式；另外如果你要用kvm， 别忘了加上--enable-kvm 选项；如果不用vnc，而使用sdl，那别忘了加选项--enable-sdl.
$make
$sudo make install
$ldd ./qemu-img 
执行这个命令是为了查看qemu-img所指向的库是否都存在！
```
# 支持操作 #
```
注意：
1.目前尚不支持prealloc操作
2.确保ldd ./qemu-img 所显示的库(hlfs,hdfs,jvm,log4c等）处于动态库搜索路径，如果不再，则要自己手动添加到路径中。
```
  * 创建网络磁盘操作
```
mkdir /tmp/testenv
./qemu-img create -f hlfs hlfs:local:///tmp/testenv/testfs 10G 或
./qemu-img create -f hlfs hlfs:hdfs:///tmp/testenv/testfs 10G
```
  * 状态查询
```
./qemu-img info -f hlfs hlfs:local:///tmp/testenv/testfs 或
./qemu-img info -f hlfs hlfs:hdfs:///tmp/testenv/testfs 
```
  * 快照相关操作
```
生成快照
./qemu-img snapshot -c snapshot1 hlfs:local:///tmp/testenv/testfs 或
./qemu-img snapshot -c snapshot1 hlfs:hdfs:///tmp/testenv/testfs
查询快照
./qemu-img snapshot -l hlfs:local:///tmp/testenv/testfs 或
./qemu-img snapshot -l hdfs:local:///tmp/testenv/testfs
删除快照
./qemu-img snapshot -d snapshot1 hlfs:local:///tmp/testenv/testfs 或
./qemu-img snapshot -d snapshot1 hlfs:hdfs:///tmp/testenv/testfs
```
  * 格式转换操作
```
./qemu-img convert raw.img  hlfs:local:///tmp/testenv/testfs 或
./qemu-img convert raw.img  hlfs:hdfs:///tmp/testenv/testfs
```
  * KVM启动
```
qemu-system-x86_64 -enable-kvm -m 1024 -smp 2 -drive file=hlfs:local:///tmp/testenv/testfs,if=virtio -boot d -vnc :7 或 
qemu-system-x86_64 -enable-kvm -m 1024 -smp 2 -drive file=hlfs:hdfs:///tmp/testenv/testfs,if=virtio -boot d -vnc :7
从某个快照上启动
qemu-system-x86_64 -enable-kvm -m 1024 -smp 2 -drive file=hlfs:local:///tmp/testenv/testfs%snapshot1,if=virtio -boot d -vnc :7 或 
qemu-system-x86_64 -enable-kvm -m 1024 -smp 2 -drive file=hlfs:hdfs:///tmp/testenv/testfs%shopshot1,if=virtio -boot d -vnc :7
%后为快照名称
```

  * Qemu 安装系统盘和启动 - 有的朋友说自己没有kvm环境，那么我们就用qemu好了！
```
1.创建10G虚拟盘：qemu-img create -f hlfs hlfs:local:///tmp/testenv/testfs 10G。
2.通过iso安装系统：qemu-system-x86_64 -hda hlfs:local:///tmp/testenv/testfs  -cdrom /home/kanghua/ubuntu-12.04.1-server-i386.iso -boot d -m 512 -no-acpi （漫长的等待...,直到完毕；我是通过本机上执行vncviewer 127.0.0.1 5900 去操作的，没有vncviewer的话，sudo apt-get install tightvnc-java去安装一个小客户端吧，登陆时密码为空！）
3.启动新安装系统 qemu-system-x86_64 -m 512 -drive file=hlfs:local:///tmp/testenv/testfs
```

  * Clone 操作
```
1.首先给base hlfs系统打个快照 - ./qemu-img snapshot -c snapshot1 hlfs:local:///tmp/testenv/testfs
2.创建新的hlfs系统 - ./qemu-img create -f hlfs hlfs:local:///tmp/testenv/testfs2 10G
3.执行clone操作 -./qemu-img create -b hlfs:local:///tmp/testenv/testfs%snapshot1 hlfs:local:///tmp/testenv/testfs2 （快照和base系统之间使用%区分！）
4.试试看hlfs:local:///tmp/testenv/testfs2啦。

--------------------------------------------

不用qemu的情况下，在我们output/bin下也有快照和clone命令，类似用法为
./output/bin/snapshot.hlfs -u local:///tmp/testenv/testfs -s snapshot1
./output/bin/clone.hlfs -f local:///tmp/testenv/testfs%snapshot1 -s local:///tmp/testenv/testfs2 

--------------------------------------------

CLONE行为用在那里？ 
A 一个hdfs之上的系统盘镜像，可以作为无数新系统的base系统，从而提高系统新系统生产速度和解决存储空间。
B 为了减少本网络传输压力，提高系统响应速度，可考虑利用本地文件系统作为Base数据宿主：比如一些场景中——我们可以将标准的镜像或某系通用软件做到工具盘中，并置于到本地hlfs上,即local方式挂载的hlfs系统上，然后再在集群的hdfs上做一个新hlfs系统，并将其base到本地上述hlfs上。从而只有变化的增量数据需要途径网络io，这样很大程度上会提高系统性能。

```