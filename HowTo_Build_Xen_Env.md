# 介绍 #
  * 在参与开发Cloudxy项目之前，需要搭建一些环境，很多想要参与的朋友可能会花费一些时间在这上面，为了让他们把精力放在项目的核心开发上，我们专门推出系列文章来说明如何快速的搭建环境。
  * HowTo\_Build\_Xen\_Evn -- 如何搭建Xen开发环境，也是本文要详细说明的部分。
  * HlfsUserManual -- 如何搭建Hlfs开发环境，这个可以参考 http://code.google.com/p/cloudxy/wiki/HlfsUserManual
  * ECMS\_RD\_Manual -- 如何搭建ECMS开发环境，这个可以参考 http://code.google.com/p/cloudxy/wiki/EMCS_RD_Manual
  * 感谢康华师哥的帮助和指导，同时也感谢康奇师哥在Xen虚拟机搭建工作上所作的基础文档，没有他们的帮助，我可能无法顺利的获取以下正确途径。

## 目的 ##

  * 在Centos 5.4 x86\_64发行版上搭建Xen虚拟机环境
  * 下载X86\_64内核镜像，并在搭建的Xen虚拟机环境下用这个内核镜像创建一个domu虚拟机。

## 具体步骤 ##

### 第一步.安装Centos 5.4 x86\_64 发行版 ###
你可以在裸机上安装，也可以在vmware station上安装。具体的安装步骤，大家应该都会。如果不是很清楚，可以在网上找找，按照步骤安装即可。选择安装包时，记住选上虚拟化相关软件包。

### 第二步.安装Xen所依赖的开发包 ###
  * yum groupinstall "Development Libraries"
  * yum groupinstall "Development Tools"
  * yum install transfig wget texi2html libaio-devel dev86 glibc-devel e2fsprogs-devel gitk mkinitrd iasl xz-devel bzip2-devel pciutils-libs pciutils-devel SDL-devel libX11-devel gtk2-devel bridge-utils PyXML qemu-common qemu-img mercurial libidn-devel
  * yum install glibc-devel.i686


### 第三步.下载Xen源码和打过补丁的内核源码以及编译内核所需的config文件 ###
  * 下载Xen源码xen-4.1.2.tar.gz (http://bits.xensource.com/oss-xen/release/4.1.2/xen-4.1.2.tar.gz)
  * 下载打过xen补丁的内核源码linux-2.6.32.39 (http://cloudxy.googlecode.com/files/kernel-xen.tar.gz)
  * 下载编译内核时所需的config文件 (http://cloudxy.googlecode.com/files/config.txt)

### 第四步.编译xen源码 ###
  * make xen
  * make tools
  * make stubdom
  * make install-xen
  * make install-tools
  * make install-stubdom
  * 本步骤完成后就会在/boot目录下生成xen-4.1.2.gz等文件

### 第五步.编译内核源码 ###
  * make menuconfig (生成config文件)
  * 需要用第三步下载到的config文件覆盖这个config文件 (注意config文件名是'.config')
  * make
  * make modules
  * make modules\_install
  * make install
  * 本步骤完成后，就会在/boot目录下生成vmlinuz-2.6.32.39和initrd-2.6.32.39.img等文件

### 第六步.配置grub启动项 ###
  * 需要在/boot/grub/grub.conf中添加以下内容
```
title jiawei-xen-4.1.2
	root (hd0,0)
	kernel /xen-4.1.2.gz
	module /vmlinuz-2.6.32.39 ro root=LABEL=/ rhgb quiet
	module /initrd-2.6.32.39.img
```
  * 在/etc/fstab中添加以下内容
none /proc/xen xenfs defaults 0 0

  * 重新启动机器，再顺序启动xencommons和xend
/etc/init.d/xencommons restart
/etc/init.d/xend restart
  * 成功启动后，运行 xm info 可以看到：
```
host                   : local00212201021a.zte.com.cn
release                : 2.6.32.39
version                : #1 SMP Thu Feb 16 18:05:02 CST 2012
machine                : x86_64
nr_cpus                : 2
nr_nodes               : 1
cores_per_socket       : 2
threads_per_core       : 1
cpu_mhz                : 1795
hw_caps                : bfebfbff:20100800:00000000:00000940:0000e31d:00000000:00000001:00000000
virt_caps              : 
total_memory           : 984
free_memory            : 513
free_cpus              : 0
xen_major              : 4
xen_minor              : 1
xen_extra              : .2
xen_caps               : xen-3.0-x86_64 xen-3.0-x86_32p 
xen_scheduler          : credit
xen_pagesize           : 4096
platform_params        : virt_start=0xffff800000000000
xen_changeset          : unavailable
xen_commandline        : 
cc_compiler            : gcc 版本 4.1.2 20080704 (Red Hat 4.1.2-51)
cc_compile_by          : root
cc_compile_domain      : zte.com.cn
cc_compile_date        : Thu Feb 16 17:14:21 CST 2012
xend_config_format     : 4
```

### 第七步. 创建domu虚拟机 ###
  * 下载domu的内核镜像 (http://cloudxy.googlecode.com/files/domU-x86_64-FS.img2.zip)。
  * 解压内核镜像文件并创建虚拟机启动配置 - pygrub.conf
```
memory = 512
name = "ttylinux"
bootloader = "/usr/bin/pygrub"
disk = ['tap2:aio:/home/jiawei/workshop1/test/domU-x86_64-FS.img,xvda1,w']
```
  * 启动domu虚拟机, 输入 xm cr pygrub.conf
  * 登陆domu虚拟机，输入 xm console ttylinux (用户名:root, 密码:password)

# 注意事项 #
  1. 如果选择使用vmware station搭建环境，最好使用vmware station 7.0或以上版本， 具体原因见 http://code.google.com/p/cloudxy/wiki/My_Test_Env_Build
  1. 搭建此环境我一直在root用户下，如果你在普通用户下，某些步骤可能需要在root用户的权限下完成。
  1. 本文所使用的linux发行版，xen源码包和打过补丁的内核源码以及其他依赖包的版本都不是固定的，但是不能保证其他版本在搭建过程中可能会出现一些问题，最好使用本文所使用的版本进行实验。
  1. 第四步在编译xen源码的过程中，需要保持网络畅通，因为编译过程中还需要下载一些依赖包。
  1. 第七步配置pygrub.conf文件时，需使用你机器上domU-x86\_64-FS.img的绝对路径。
  1. 如果在第七步在创建domu虚拟机过程中出现问题，请查看/var/log/xen/xend.log分析原因。
  1. 由于本人水平有限，如果你在实验的过程中发现某些步骤不是很合理或者存在问题，请及时指正。
  1. 抛砖引玉，搭建xen环境的方法很多，请理解为主，不要拘泥。


---

written by 贾威威 2012 2 17