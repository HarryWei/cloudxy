# Introduction #

虚拟机启动流程简述


# Details #

Add your content here.  Format your content with:
  * Text in **bold** or _italic_
  * Headings, paragraphs, and lists
  * Automatic links to other wiki pages
在描述所有细节之前，先展示一下目录结构：


|-- baseimage
|   |-- domU-32bit-FS.img
|   `-- domU-64bit-FS.img
|-- hlfs\_tools
|   |-- bin32
|   |   |-- mkfs.hlfs
|   |   |-- nbd-client
|   |   `-- nbd-server
|   |-- bin64
|   |   |-- mkfs.hlfs
|   |   |-- nbd-client
|   |   `-- nbd-server
|   |-- lib32
|   |   |-- libhdfs.so.0
|   |   |-- libhlfs.so
|   |   |-- libjvm.so
|   |   `-- liblog4c.so.3
|   `-- lib64
|       |-- libglib-2.0.so.0
|       |-- libhdfs.so.0
|       |-- libhlfs.so
|       |-- libjvm.so
|       `-- liblog4c.so.3
|-- priv
|   |-- domU-32bit-FS.img.tgz
|   |-- domU-64bit-FS.img.tgz
|   |-- hlfs\_tools
|   |   |-- bin32
|   |   |   |-- mkfs.hlfs
|   |   |   |-- nbd-client
|   |   |   `-- nbd-server
|   |   |-- bin64
|   |   |   |-- mkfs.hlfs
|   |   |   |-- nbd-client
|   |   |   `-- nbd-server
|   |   |-- lib32
|   |   |   |-- libhdfs.so.0
|   |   |   |-- libhlfs.so
|   |   |   |-- libjvm.so
|   |   |   `-- liblog4c.so.3
|   |   `-- lib64
|   |       |-- libglib-2.0.so.0
|   |       |-- libhdfs.so.0
|   |       |-- libhlfs.so
|   |       |-- libjvm.so
|   |       `-- liblog4c.so.3
|   `-- script
|       `-- vm\_ops\_scripts
|           |-- add\_vdisk
|           |   `-- vdisk\_add.bash
|           |-- destroy\_vm
|           |-- restart\_vm
|           |   `-- vm\_restart.bash
|           |-- start\_vm
|           |   |-- scene.bash
|           |   `-- vm\_start.bash
|           |-- stop\_vm
|           |   `-- vm\_stop.bash
|           |-- utils
|           |   |-- env.bash
|           |   |-- hlfs\_ops.bash
|           |   |-- log.bash
|           |   |-- misc.bash
|           |   |-- nbd\_ops.bash
|           |   |-- vdisk\_ops.bash
|           |   |-- vhd\_ops.bash
|           |   `-- vm\_ops.bash
|           `-- vdisk\_attach
|-- vm\_ops\_scripts
|   |-- add\_vdisk
|   |   `-- vdisk\_add.bash
|   |-- destroy\_vm
|   |-- restart\_vm
|   |   `-- vm\_restart.bash
|   |-- start\_vm
|   |   |-- scene.bash
|   |   `-- vm\_start.bash
|   |-- stop\_vm
|   |   `-- vm\_stop.bash
|   |-- utils
|   |   |-- env.bash
|   |   |-- hlfs\_ops.bash
|   |   |-- log.bash
|   |   |-- misc.bash
|   |   |-- nbd\_ops.bash
|   |   |-- vdisk\_ops.bash
|   |   |-- vhd\_ops.bash
|   |   `-- vm\_ops.bash
|   `-- vdisk\_attach
`-- vms\_work\_dir
> `-- VM-77
> > |-- initialize
> > |-- sysdisk
> > |   |-- diff.img
> > |   |-- lost+found
> > |   `-- vm-77.cfg
> > `-- vm-77-scene.iso






vm\_start.sh


1、通过引入env.bash 和 scene.bash 来部署环境变量及引入预定义函数，前者与环境变量相关，后者与场景化相关。

2、定义回滚函数，确保发生任何错误的情况下，程序可以回滚到运行程序之前的状态。


> xm destroy vm-$VM\_ID >/dev/null 2>&1 #确保在发生错误的时候将标准错误输出到屏幕上，正常运行不打印任何内容，防止被调

度程序读取。
> sleep 2 #睡眠一会儿，确保Dom-U被destroy后释放虚拟设备资源。

> unbind nbd设备与$URL描述的hlfs文件系统；destroy hlfs文件系统。

3、读入参数包含场景化参数所在的iso文件信息，这里场景化信息的内容，由前面参数列表中包含的内容指定。

4、检查是否有同名Dom-U正在运行，是否有与$URL同名hlfs文件系统已经存在，如果存在任意一个，表示与预期不符（因为这两者都要在后

面的过程创建），退出运行。

5、为Dom-U运行创建本地工作目录，用于准备启动配置文件，快照文件以及场景化iso之用。

6、使用url创建hlfs文件系统，这里可以通俗的将其理解为为一块新硬盘做低级格式化，生成用于磁盘管理的基本信息。

7、加锁；自动选择空闲nbd设备与$URL指代的hlfs文件系统bind，可以通俗的理解为，将一块新的，做过低级格式化的硬盘，连接到本地节

点。由于涉及到nbd块设备的分配和使用问题，所以
这个过程需要在临界区内执行，以防多个线程争夺同一个nbd块设备。


8、为nbd块设备准备文件系统，可以通俗的理解为将前面连接到系统的硬盘高级格式化为ext3文件系统。

9、将格式化好的的$NBD设备mount在相应Dom-U工作目录下的系统盘目录，这里是$SYSDISK ，最后解除前面对临界区加的共享互斥锁，因为

涉及到$NBD设备的使用到这里已经全部完成了。前面的几个步骤，如果有任何问题导致退出都不要忘了最后要做解锁工作。

10、使用vhd-util工具，在$SYSDISK创建系统增量盘映像文件，根据$VM\_OS\_TYPE确定系统母盘映像文件。


11、在$SYSDISK创建对应Dom-U启动配置文件：$SYSDISK/vm-$VMID.cfg，包括Name、CPU、Memory、boot、Disk、IP、VNC等信息。
> name = "vm-$VMID"
> memory = $VMMEM
> vcpu = 1
> bootloader="/usr/bin/pygrub"
> disk = ["tap2:vhd:$DIFFIMAGE\_PATH,xvda1,w" ](.md)
> vif  = ["ip=$VMIP"](.md)
> vfb  = ["type=vnc,vncunused=$((VNCPORT-5900)),vncpasswd=$VNCPASSWD" ](.md)

12、使用mkisofs创建场景化iso映像文件，内容包括IP地址，主机名，root口令。
> local scene\_file=`dirname $scene_iso_file`/initialize
> echo "hostname $vm\_hostname" > $scene\_file;
> echo "password $vm\_passwd" >> $scene\_file;
> echo "ipaddr $vm\_ipaddr" >> $scene\_file;
  1. at $scene\_file
> mkisofs -r -o $scene\_iso\_file $scene\_file >/dev/null 2>&1;

13、将场景化光盘连接到对应Dom-U的cd设备上。
> xm block-attach $VMNAME file:$ISO\_FILE\_PATH xvdd:cdrom r >/dev/null 2>&1

14、通知对应Dom-U，场景化滚光盘已经就位。
> xenstore-write /local/domain/$VMID/iso\_attach 1
> > 并将对应的访问域置为监听状态。

> xenstore-chmod /local/domain/$VMID/iso\_attach b

15、等待Dom-U场景化动作完成并将访问域flag置位，通知Dom-0。Dom-0会循环20次，间隔2秒去扫描xenstore-read /local/domain/

$VMID/iso\_attach域。



16、read
> 这条指令字面的作用是可以将输入赋值给$REPLY，但是就像你看到的，既没有把你的输入当作命令来执行，也没有打印这个$REPLY。
这里的实质是康sir秀了一个编程的技巧，如果场景化动作失败，比如说循环20次还没有读到场景化完成的消息，在回退，清除痕迹之前暂

停一下，让你可以进入Dom-U中查看原因：P


17、将场景化iso文件与相关Dom-U断开连接。







vm\_stop.bash

1、通过引入env.bash 和 scene.bash 来部署环境变量及引入预定义函数，前者与环境变量相关，后者与场景化相关。

2、定义回滚函数，确保发生任何错误的情况下，程序可以回滚到运行程序之前的状态。

3、首先检查对应Dom-U是否正在运行，if xm domid $VM\_NAME；如果正在运行就向其发关机指令，xm des $VM\_NAME。

4、查找与对应Dom-U关联的工作目录与哪个nbd设备相关，NBD=`mount -l|grep "\/$VM_DIR\/"|cut -d" " -f1`。

5、等待3秒后（等待tapdisk释放vhd设备，不再使用nbd设备）,unmount $NBD, unbind /dev/nbd-?，

为了清晰说明卸载过程，下面列出Dom-U所使用的系统硬盘的装载过程：
> mkfs.hlfs		—>	URL@hlfs
> bind&mkfs.ext3		—>	/dev/nbd-?
> mount			—>	$SYSDISK（workdir/dom-U/sysdisk）
> vhd-util snapshot	—>	$DIFFIMAGE（vhd）
> tapdisk			—>     hda@Dom-U

6、清除tapdisk？ 本意是处理xen的一个bug，强行关机后tapdisk设备没有被release掉，我好想也观察到过一次，不过好像只是时间的问

题，我不干确定那次是我没有调用vm\_stop.bash之后还是之前看到的。不过这一没有发生作用，且因为地址没有完全对应而没有得到执行。

但是前面说的那个问题的确没有出现现，还得需要康sir给大伙扫下盲。

> function destroy\_blktap\_dev(){
> if [$# != 1 ](.md); then
> > LOG\_MSG  "$