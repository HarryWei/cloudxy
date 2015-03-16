## Xen母盘和增量盘的制作 ##

  * qcow的母盘使用:qemu-img convert -f raw name -O qcow name来制作。
  * qcow的增量盘使用上面的方法：qcow-create来制作
  * vhd的母盘最好使用window的virtual pc来做
  * vhd的增量盘制作：vhd-util snapshot -n childname -p parentname -m(if is raw)

## 虚拟机场景化实现 ##
  * 原因
用户虚拟机镜像是在申请时现场创建，而且该系统镜像是base在预装母盘上的增量盘。由此可见，创建的所有的镜像内容和母盘相同，但各用户的系统则有其差异性，比如每个用户虚拟机都有各自不用的ip地址、不同的主机名、不同的密码。
  * 方式
目前场景化做法多采用，置入一个简易的启动自运行脚本方式实现自我设置。
这个脚本在虚拟机启动后，自动执行——比如设置hostname,设置密码、设置ip等。<br>这个场景化脚本必须足够简单、完全开放——因为要被客户审核，以取信于用户。</li></ul>

<ul><li>场景化数据如何在VM中获得<br>
以我们上面的简单场景化来说，我们需要ip地址、需要hostname、需要密码等信息。<br>
那么现在的问题是这些信息如何从vm中获得（我们知道这些信息来自master给定）？</li></ul>

<h2>可选的方案 ##

  1. 网络获取方式
  * dom0里配置dhcp或者集群中配置一个统一的dhcp服务，VM启动后自动获得一个ip地址。
  * vm启动后，其内的自动运行的场景化脚本通过网络到dom0中，或者到系统中指定的ftp等服务去拉去各自对应的配置信息，进行自我配置。

> 2. 动态盘方式获取方式
  * dom0接到master下发的配置信息（ip/passwd/hostname)——通过启动命令传递给dom0,或者dom0去系统中某个ftp拉取.
  * 做一个很小的ISO文件系统，并将配置信息以文件形式放入该ISO文件系统.
  * 启动虚拟机，将这个iso只读模式动态挂载到vm上（block-attach命令）—— 这时在vm中能看到且能访问该盘。
  * VM启动后，场景化脚本从该iso读取配置信息进行自我配置。
  * dom0等VM配置完成后，卸载该iso盘,并销毁该iso。


## 注意的问题 ##
  * 动态挂载和卸载ISO —— 这个不难，掌握block-attach等命令即可完成。
  * 场景化脚本执行触发—— 这个脚本在只做母盘是置入系统即可，要能自启动，比如至于rc.local下。

  * 场景化脚本自我禁止—— 这个脚本只在第一系统启动执行一次（设置一次密码，主机名即可），所以其逻辑要能判断是第几次启动，以决定是否需要配置任务。（禁止方法多种多样，可检测配置是否已经有。也可运行第一次成功后，最后自我删除）

  * 动态卸载触发问题—— 当场景化脚本设置完毕后，我们最好能自动卸载掉iso。由于卸载动作在dom0执行，而场景化工作由vm内执行。所以vm执行完毕后需要通知dom0进行执行完毕，dom0才能卸载iso。这个通知动作是个技术点——半虚拟化可采用xenstore完成（vm中设置某个key value表示执行完毕，dom0里检查这个值，如果发现设置了，则说明场景化工作已经执行完毕，可以进行卸载）
```
示例脚本：
mkisofs -o /tmp/$1.iso /Xen/iso > /dev/null 1>/dev/null 2>&1
rm -rf ${iso_config}*
#ls /tmp/$1.iso
 
#xm list
echo -e "\n\tID is $ID\n"
echo -e "\n\tName  is $1\n"
 
#read -p "Right? : [y/n]" choice
#if [ "$choice" == "y" ];then
#       echo -e "\nStill GO....\n"
#else
#       exit 101
#fi
 
xm block-attach 1.iso xvdd r > /dev/null 2>/dev/null
```

## xenstore，虚拟机的通信 ##
  * 什么是xenstore？
Xenstore 是主要的用来控制建立来自客户机的共享内存区域的事件通道、管理客户机的通知，收集客户机状态数据的工具。
说白了就是主要的作用就是虚拟机之间的通信。

  * xenstore 目录结构：
Xenstore 的目录是层次似的key-data 值对。每个域都有一个目录继承结构，包含存储它自身的信息，主要是配置信息。
这些信息一般存储于一个数据库中，位于/var/lib/xenstored/tdb。
是个数据库没错，但是也有说是文件系统的，我们这里管他是什么呢，用到哪里说哪里。

**内部的3 个主要的路径：
  1. /vm：包含和域相关的配置；
  1. /local/domain：本节点上存储的信息；
  1. /tool：包含不同用户模态下的工具。**

```
1.注意Xenstore工具的底层通讯机制需要pv支持，如果你是的DOMU是半虚拟化的，那么直接有了；但如果是HVM/SVM全虚拟化的，则需要安装pv driver。
2.Xenstore是个工具，如果你有pv支持啦，那么只需安装该工具即可xen/tools/xenstore 编译安装即可。
3.如果还嫌自己编译费劲，再交给大家一个偷懒的办法! 直接将dom0中的/usr/lib/xenstore* 文件和ldd /usr/lib/xenstore看到的库加到domU镜像就可以了。
```

## 相关命令 ##
你可以使用这些信息来开发一个定制的管理接口或者进行Xen 环境下简单的查询。下面几个命令是和此有关的：
  * xenstore-chmod：准许管理员和开发人员手工改变Xenstore 中任意位置的权限—— w(write only)/r(read only)/b(both read and write)/n(no access)；
  * xenstore-list：列出存放于目录中存放的key，或者类别。
  * xenstore-read：输出和一个特定key 有关的值。
  * xenstore-write：改变key 中存放的值。
  * xenstore-exists：用于检查一个特定的key-value 是否存在。
  * xenstore-ls：输出整个数据库树，包含所有的节点和key-value 值对。
  * xenstore-rm：删除一个key。

## 下来我们实战应用一下 ##
```
root @ 147 ~ 15: xenstore-write /local/domain/19/key 1   ###在虚拟机中新建一个key变量，值初始化成1，注意这个key需要使用xenstore-chmod修改权限的。

root @ 147 ~ 16: xenstore-read /local/domain/19/key    ##看看这个值
1
root @ 147 ~ 17: xm console 19
root @ mother / 69: ##切到虚拟机中
root @ mother / 69: 
root @ mother / 69: ls
bin   dev  home  lib64      media  mnt  opt   root  selinux  sys  usr Xenboot  etc  lib  lost+found  misc   net  proc  sbin  srv      tmp  var
root @ mother / 70: xenstore-write key 3           ##在虚拟机中修改这个值
root @ mother / 71: 
root @ 147 ~ 18: 
root @ 147 ~ 18: ##切到主机
root @ 147 ~ 18: 
root @ 147 ~ 18: 
root @ 147 ~ 18: ##下面查看这个值
root @ 147 ~ 18: xenstore-read /local/domain/19/key   
3
我们可以发现在虚拟机内部修改的一个变量key的值，在虚拟机的外部（host）中可以看到修改后的key的值。
```

```
1. 这里要补充一点。 domU中设置key前，需要在dom0中先修改key权限为b - xenstore-chmod /local/domain/19/key b
2. 需要的话可以从download中下载domU-64bit-withxenstore-FS.img.tar.gz  
```


---

Written by 田康奇(kangqi1988@gmail.com ) 贾威威 (harryxiyou@gmail.com）<br>
2012, 2, 25