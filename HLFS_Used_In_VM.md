# HLFS虚拟机中使用演示 #
很多同学对虚拟机运行期间各种操作概念不熟悉，更对HLFS不熟悉。所以在我们将虚拟机操作集成到ECMS管理体系前，很有必要给大家做一个"虚拟机+HLFS使用"的demo。详细梳理一下虚拟机管理、虚拟磁盘使用（系统盘创建、数据盘创建、增量盘创建等）、DOMU场景化等过程。当你掌握了这些过程，无论对虚拟机还是HLFS的理论理解和实际操作使用能力都将提升到一个新的层次。

# 本文将涉及到如下任务 #
  1. 环境准备——含虚拟机控制脚本、母盘部署、HLFS工具部署
  1. 虚拟操作——包含：
    * 虚拟机启动 —— 需要基于母盘创建增量盘作为DomU的系统盘、需要将DomU系统盘放在基于HLFS的分布式网盘上、需要在首次启动时对DomU进行场景化设置
    * 虚拟机关闭
    * 虚拟机重启 —— 根据给定系统所在母盘的HLFS URI地址，重新挂载母盘和启动虚拟机。
    * 虚拟网盘动态挂载 ——  生成空的虚拟数据网盘，并挂载到给定虚拟机上。

# 演示步骤 #
前提：你已经安装了xen4.1之上的代码(建议你从源码安装），且tap-ctl vhd-util等工具都已经具备，而且在$PATH路径下。
### 环境部署 ###
  1. 安装虚拟机控制脚本
  * su root #我们暂时都使用root操作,如果你没打开root帐号，则sudo -i使用临时root权限
  * svn co http://cloudxy.googlecode.com/svn/trunk/ECMS/NodeController/priv /opt/priv
  * 部署母盘baseimg
    * mkdir /opt/baseimage
    * tar zvxf /opt/priv/domU-64bit-FS.img.tgz -C /opt/baseimage
    * tar zvxf /opt/priv/domU-32bit-FS.img.tgz -C /opt/baseimage
  * 部署虚拟机操作脚本
    * cp /opt/priv/script/vm\_ops\_scripts /opt -rf  #opt目录是目前脚本配置设定的
  * 部署hlfs工具
    * cp /opt/priv/hlfs\_tools /opt -rf  #hlfs\_tools下包含了hlfs需要的执行程序和对应的依赖库。

### 虚拟机操作部分 ###
cd /opt/vm\_ops\_scripts
  * 启动vmid=77的虚拟机
    * bash -x start\_vm/vm\_start.bash 77 local:///tmp/testfs 64 vncpass 5900 hostname vmpass 127.0.0.1 32bit
```
 参数：
 77：是一个全局虚拟机id号，不是xm list看到的虚拟机id
 local:///tmp/testfs 是系统盘的地址uri
 64：是待给虚拟机分配的内存大小
 vncpass:是vnc访问密码
 5900:是vnc端口号（注意，同一机器上该端口号不能重复）
 hostname：是待启动虚拟机指定的主机名，将在场景化过程中设置给虚拟机。
 vmpass:是待启动虚拟机指定的root密码，将在场景化过程中设置给虚拟机。
 127.0.0.1:是待启动虚拟机指定的IP地址，将在场景化过程中设置给虚拟机。
 32bit:是待启动虚拟机镜像类型。目前还支持64bit。 -- 
 ISSUE: 很奇怪的是 - 32bit的镜像使用passwd<<EO F方式修改密码不成功，64bit镜像缺可以。原因还待调查。所以32bit登陆时还是用母盘密码password吧。
 
```

```
说明：
 1.执行成功后在dom0上执行xm console vm-77 登陆，此时root密码是vmpass,且 hostname是hostname,ip是127.0.0.1 - 场景化执行完毕,虚拟机已经在网盘NBDX上启动——你可到/tmp/testfs目录下看到hlfs产生的段文件。
 2.我们系统盘使用的改造后的NBD方式运行的网盘做增量文件的宿主块设备。ps -aux|grep nbd 可看到对应的nbd-server和nbd-client.
```

  * 关闭vmid=77的虚拟机
    * bash -x stop\_vm/vm\_stop.bash 77
  * 重新启动vmid=77的虚拟机
    * bash -x restart\_vm/vm\_restart.bash 77 local:///tmp/testfs
  * 为vmid=77的虚拟机加入虚拟磁盘local:///tmp/datafs --- 这步需要给xen打patch后才能支持
    * bash -x add\_vdisk/vdisk\_add.bash 77 local:///tmp/datafs 64
```
   注意
   1.虚拟数据盘实现是通过在tapdisk2中增加了一个hlfs-block的驱动实现的，和NBD实现的网盘有类似之处，但IO流上来讲直接从tapdisk2的用户态走了，而NBD则IO经tapdisk2后还要再转到内核态nbd device，然后再转化到nbd-server用户态才能走,所以更精炼。
   2.此刻我们网盘是在本地文件系统模拟，如果安装hadoop后，则可在hadoop上实现,也就是说将local标识换成hdfs即可
```