# 介绍 #

对于入门级的个人开发者来说，如何最简单的开发和实验cloudxy是大家头痛的问题。我以我个人的开发环境来说一下如何搭建这个环境！

## 目的 ##

  * 能运行pv domU，能运行hlfs，能运行elrang
  * 最好还能在window上开发(毕竟多数人是用window上网，游戏）

## 具体步骤 ##

### 第一步.安装VMware ###
下载vmware station 7 (我用的是7.1.4 build-385536，注册码网上有的是）。
之所以使用vmware station 7是因为它支持在“32位宿主机上安装64未虚拟机”,当然前提是你宿主机要支持VT（这个一般要在bios里打开，如果你机器支持的话）
不过，即便你机器没有VT也不要紧，因为安装32位的宿主机我们的系统一样运行。

### 第二步.安装宿主机 ###
下载ubuntu server iso(我用的是Ubuntu 10.10 ，即Maverick Meerkat)，然后在vmware中安装。当然，你要记得安装后续需要的包，如apt-get install build-essential等，总之碰到缺少的就安装好了。 另外说明一下，我给vmware的宿主机中分配了2G内存，如果你要跑hadoop等就最少要给1G以上。

### 第三步.安装Xen ###
安装xen！对于ubuntu 而言，你只需简单执行sudo apt-get install xen-hypervisor-4.0 即可（将安装Linux 2.6.32-5-xen-amd64 and XEN 4.1.1在你系统里）重启grub中选择对应项就可进入xen环境啦 —— 我们最终是在vmware中的宿主机上再启动虚拟机，即所谓的virtualization stack
```
   1.如果刚安装完server版本，要先sudo apt-get update一下，已更新仓库。 
   2.如果你用Maverick 版本。那么仓库里的xen-hypervisor是3x版本，所以还需要手动添加一个源，才能安装4版本，操作如下：
    sudo add-apt-repository ppa:ukplc-team/xen-stable
    sudo apt-get update
    sudo apt-get install ubuntu-xen-server
```

### 第四步.创建domU ###
启动虚拟机看看
先可下载我们download中的 pv domU ——　见http://code.google.com/p/cloudxy/wiki/HowTo_Build_PV_domU 然后尝试本地镜像方式启动domU。

### 第五步.编译hlfs等 ###
若要是尝试hlfs，则需要按照http://code.google.com/p/cloudxy/wiki/HlfsUserManual  所说执行。你应可先尝试一下“HLFS 存储虚拟机镜像”中的local模式；然后再是hdfs模式。

### 第六步.安装erlang ###
若要是尝试EMCS，则需要按照http://code.google.com/p/cloudxy/wiki/EMCS_RD_Manual 所说执行。注意erlang要下载OTP R15B版本，因为默认erlang版本可能有些地方不支持。


### 第七步. ###
利用hlfs启动虚拟机 -- 先买个关子，留给大家思考


---

wroten by 康华