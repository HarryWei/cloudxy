# 快速部署（性急的只需看本节，欲知详情请阅读后面内容） #
0.安装Ubuntu 12.04发行版，源更新为国内较快源(网易或者搜狐等，学校就用电子科大等源)

1.下载devstack,并打上我们提供的patch
```
sudo apt-get install git (如果你还没有安装git）
git clone git://github.com/openstack-dev/devstack.git 
cd devstack;git reset --hard 25ebbcd1a7bc95e69ad32b19245ce0990a29eaa7 
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_devstack_goback.patch 
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_devstack.patch 
git apply hlfs_driver_for_devstack_goback.patch 
git apply hlfs_driver_for_devstack.patch 
```

2.启动stack.sh，其过程会下载openstack
```
./stack.sh  （需要等待一段时间，完成后执行./unstack）
-------------------------------------------------
注：如果是国内用户，那么可以考虑将python源换成国内的，以保证python相关包下载顺利——增加pip.conf文件，内容如下即可
$ cat ~/.pip/pip.conf
[global]
timeout = 6000
index-url = http://e.pypi.python.org/simple
[install]
use-mirrors = true
mirrors = http://e.pypi.python.org
-------------------------------------------------
```

3.给openstack的源码部分打patch
```
cd /opt/stack/nova
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_openstack_nova.patch
git apply hlfs_driver_for_openstack_nova.patch
cd /opt/stack/cinder
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_openstack_cinder.patch
git apply hlfs_driver_for_openstack_cinder.patch
```

4.一键安装所有hlfs相关的包（java,hlfs,hadoop,qemu,libvirt等）
```
sudo apt-get install subversion
svn checkout http://cloudxy.googlecode.com/svn/branches/hlfs/person/zhangdianbo/scripts scripts;
cd scripts;./hlfs.sh
```

5.重启devstack——以hlfs作为cinder默认存储
```
CINDER_DRIVER=hlfs ./stack.sh
-------------------------------------------------
如果下面要通过命令行创建HLFS volumes(或者对HLFS做一些operations)需要在/etc/profile中增加环境变量，具体如下：

#Openstack ENV.
export OS_TENANT_NAME=admin
export OS_USERNAME=admin
export OS_PASSWORD=xxxxx(这里换成开始安装devstack输入的密码)
export OS_AUTH_URL="http://(这里换成devstack安装结束提示的ip):5000/v2.0/"
export EC2_URL=$(keystone catalog --service ec2 | awk '/ publicURL / { print $4 }')
export CREDS=$(keystone ec2-credentials-create)
export EC2_ACCESS_KEY=$(echo "$CREDS" | awk '/ access / { print $4 }')
export EC2_SECRET_KEY=$(echo "$CREDS" | awk '/ secret / { print $4 }')
-------------------------------------------------
```

注：目前都是patch形式维护我们集成代码，待社区接受啦！


---

# 简介 #
  * Openstack是由NASA和RACKSPACE发起的，是一款优秀的IAAS软件，可以通过http://www.openstack.org 查看有关Openstack更详细的信息。
  * Openstack包含很多子项目，要部署安装Openstack比较繁琐( http://docs.openstack.org/folsom/basic-install/content )，还好RACKSPACE开发了一键安装项目Devstack，HLFS也是通过Devstack部署和安装Openstack。
  * 目前HLFS已经支持Openstack了，下面主要介绍如何部署和安装Openstack(包含HLFS驱动)。

# 部署和安装 #
> #### 安装操作系统 ####
```
1，安装Ubuntu 12.04发行版。
2，安装基本开发环境
sudo apt-get update （执行软件列表更新前，最好先选择国内源，比如163网易的源\sohu的源 - 源设置参见http://blog.ubuntusoft.com/ubuntu-update-source.html）
sudo apt-get install build-essential (基本编译环境）
sudo apt-get install git subversion  (版本管理工具）
sudo apt-get install cmake (make管理工具）
sudo apt-get install ubuntu-desktop (桌面环境，不是必要的，但很多朋友喜欢桌面环境）
sudo apt-get install tightvnc-java  (vnc)

注意：
1， Devstack目前支持Ubuntu 12.04。
2， Ubuntu 12.04(只支持64位)支持CDH4比较完善(支持append和hflush)，同时HLFS本身不用再维护第三方库(可以本地安装libhdfs, liblog4c, libsnappy等)。
```
> #### 安装HLFS ####
```
1，安装 HLFS 依赖的库
sudo aptitude install libglib2.0-dev
sudo apt-get install libsnappy-dev
sudo apt-get install liblog4c-dev

2，安装Sun Java7 (也可安装sun-java-6)
sudo add-apt-repository "deb http://ppa.launchpad.net/webupd8team/java/ubuntu precise main"
sudo apt-get update
sudo apt-get install oracle-java7-installer 
如果想安装Sun-java-6,可按如下步骤进行
sudo apt-get install python-software-properties
sudo add-apt-repository "deb http://us.archive.ubuntu.com/ubuntu/ hardy multiverse"
sudo apt-get update
sudo apt-get install sun-java6-jdk sun-java6-jre

3, 配置环境变量
在/etc/profile中增加
####
JAVA_HOME=/usr/lib/jvm/java-6-sun (如果是sun java7 换成对应的目录)
LD_LIBRARY_PATH=$JAVA_HOME/jre/lib/amd64/server/ （或$JAVA_HOME/jre/lib/i386/server/,这取决于是在64位机还是32位机上) 
export LD_LIBRARY_PATH
####

>source /etc/profile  (使其生效)

4， 安装Hadoop环境
wget http://archive.cloudera.com/cdh4/one-click-install/precise/amd64/cdh4-repository_1.0_all.deb
sudo dpkg -i cdh4-repository_1.0_all.deb
curl -s http://archive.cloudera.com/cdh4/ubuntu/precise/amd64/cdh/archive.key | sudo apt-key add -
sudo apt-get update
sudo apt-get install hadoop-0.20-conf-pseudo
sudo -u hdfs hdfs namenode -format   （格式化namenode）
格式化完，可以看到CLASSPATH，把此变量后跟的所对应的jar包路径（复制拷贝），放入/etc/profile(如 export CLASSPATH=复制拷贝的jar包路径，最后记着source /etc/profile 使其生效)，最后启动hdfs服务。
for service in /etc/init.d/hadoop-hdfs-*
> do
> sudo $service start
> done

5，编译安装HLFS
sudo apt-get install libhdfs0-dev
svn checkout https://cloudxy.googlecode.com/svn/trunk/hlfs/ hlfs （目前已经没有3part目录了，也不用再手动修改cmakelist.txt中路径了）

cd hlfs/build

cmake -DCMAKE_INSTALL_PREFIX=/usr/local/lib ../src (在给定位置——我们给定到/usr/local/lib目录下，生成makefile文件）

sudo make install (头文件和lib,bin等都被安装到/usr/lib/hlfs目录下；如果编译出现 “usr/include/glib-2.0/glib/gtypes.h:34:24: fatal error: glibconfig.h: No such file or directory” ，别慌，执行 sudo cp /usr/lib/i386-linux-gnu/glib-2.0/include/glibconfig.h /usr/include/glib-2.0/ 或者 执行sudo cp /usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h /usr/include/glib-2.0/ 即可.）

在/etc/profile中

配置LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/hlfs/lib

export LD_LIBRARY_PATH

>source /etc/profile


注意：
最好把/usr/local/lib/hlfs/lib和/usr/lib/jvm/java-6-sun/jre/lib/amd64/server/ (32位机对应目录是/usr/lib/jvm/java-6-sun/jre/lib/i386/server/ )放入 /etc/ld.so.conf 并 sudo ldconfig使其生效。(原因是ldconfig的范围更大，包含fork子进程等等)
```

> #### 安装QEMU ####
```
git clone git://git.qemu.org/qemu.git

cd qemu; git reset --hard v1.3.0
wget http://cloudxy.googlecode.com/svn/trunk/hlfs/patches/hlfs_driver_for_qemu_1.3.0.patch

git apply hlfs_driver_for_qemu.patch
export HLFS_INSTALL_PATH=/usr/local/lib/hlfs (这个环境变量一定要设置，指向hlfs的安装目录)
./configure --enable-hlfs --with-coroutine=gthread (configure输出的最后会提示hlfs是否enable，应该是yes！)

make;sudo make install
```

> #### 安装Openstack ####
```
1，下载Devstack
git clone git://github.com/openstack-dev/devstack.git
cd devstack
git reset --hard 25ebbcd1a7bc95e69ad32b19245ce0990a29eaa7

2, 安装HLFS patches for Devstack
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_devstack_goback.patch
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_devstack.patch
git apply hlfs_driver_for_devstack_goback.patch
git apply hlfs_driver_for_devstack.patch

3, 安装Openstack
./stack.sh  （需要等待一段时间）

4，停止Openstack服务
./unstack.sh

注意：
1，安装Openstack过程中，如果网络环境不好，无法获取与pip源相关的python包，可以创建~/.pip/pip.conf文件，里面内容如下：
[global]
timeout = 6000
index-url = http://e.pypi.python.org/simple
[install]
use-mirrors = true
mirrors = http://e.pypi.python.org
2，如果有几个python包还是无法下载，那就手动安装（sudo apt-get install 或者 sudo easy-install等）
3，最后停止Openstack服务,是因为Devstack运行过程中下载的libvirt不支持HLFS，下面我们安装基于HLFS驱动的libvirt，最后在启动Openstack。
4，如果安装Openstack过程中还遇到其他问题，可以发邮件到openstack@lists.launchpad.net和cloudxy@googlegroups.com讨论。
```

> #### 安装Libvirt，重新启动基于HLFS驱动的Openstack ####
```
1, 下载Libvirt的依赖包
sudo apt-get install libtool
sudo apt-get install autoconf
sudo apt-get install automake
sudo apt-get install autopoint
sudo apt-get install xsltproc
sudo aptitude install libdevmapper-dev   -- 降级处理
sudo apt-get install libpciaccess-dev 
sudo apt-get install libnl-dev 
sudo aptitude install libgcrypt11-dev    -- 降级处理
sudo apt-get install w3c-dtd-xhtml 
sudo aptitude install libgnutls28-dev    -- 降级处理
sudo apt-get install libyajl-dev
sudo apt-get install gettext

2， 下载libvirt并打上HLFS patches for Libvirt
git clone git://libvirt.org/libvirt.git；
cd libvirt;git reset --hard v1.0.1
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_libvirt_network_disk.patch
git apply hlfs_driver_for_libvirt_network_disk.patch
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_libvirt_offline_storage.patch
git apply hlfs_driver_for_libvirt_offline_storage.patch
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_libvirt_add_classpath.patch
git apply hlfs_driver_for_libvirt_add_classpath.patch

3, 编译安装Libvirt
./autogen.sh
./configure 
./make; 
./sudo make install

4，安装HLFS patches for Openstack
cd /opt/stack/nova
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_openstack_nova.patch
git apply hlfs_driver_for_openstack_nova.patch
cd /opt/stack/cinder
wget http://cloudxy.googlecode.com/svn/branches/hlfs/features/multi-file/patches/hlfs_driver_for_openstack_cinder.patch
git apply hlfs_driver_for_openstack_cinder.patch

5, 重新启动Openstack
cd Devstack_DIR (切换到Devstack目录下)
CINDER_DRIVER=hlfs ./stack.sh

注意：如果重启Opestack过程中,遇到'keystone did not start'问题，可以通过如下方式解决：
sudo chmod -R 777 /usr/local/lib/python2.7/dist-packages
此问题是权限问题导致的。
```

# 支持操作 #
#### 创建HLFS Volumes ####
```
1, local模式下创建HLFS Volumes
cinder create --display-name local:///tmp/testenv/testfs 5
2, hdfs模式下创建HLFS Volumes
cinder create --display-name hdfs:///tmp/testenv/testfs 5

注意：
1，目前要创建HLFS Volumes --display-name选项是必须的，不可省略。
2，最后的数字是Volumes的大小，默认单位是 G。
```
#### 删除HLFS Volumes ####
```
1, 配置环境变量
给/etc/profile文件中加入如下语句
export PATH=$PATH:/usr/local/lib/hlfs/bin/  (此路径根据HLFS的具体安装路径)
最后 > source /etc/profile 使其生效

2，删除HLFS Volumes
cinder delete vol_id

注意：
1，配置环境变量的原因是cinder会间接调用rmfs.hlfs删除HLFS volumes
2, cinder的删除操作必须给出volume id，可通过 cinder list 查看。
```
#### 创建HLFS snapshot ####
```
1，创建local模式下的HLFS snapshot
cinder snapshot-create --display-name local:///tmp/testenv/testfs vol_id
2, 创建hdfs模式下的HLFS snapshot
cinder snapshot-create --display-name hdfs:///tmp/testenv/testfs vol_id

注意：
1， display-name后跟参数与创建HLFS volumes的display-name要一致。
2， vol_id是要创建snapshot的HLFS Volumes id，可通过cinder list 查看。
3， 最后可通过 cinder snapshot-list 查看创建的快照，如果成功status会显示available。
```
#### 删除HLFS snapshot ####
```
cinder snapshot-delete snapshot_id

注意：
snapshot_id可以通过cinder snapshot-list 查看。
```

# 注意事项 #
  1. 目前HLFS支持Openstack并不完善，后期会继续更新。
  1. 搭建环境过程中，可能会遇到这样那样的问题，如果实在无法解决们可以发邮件到我们群邮件(cloudxy@googlegroups.com)寻求帮助。
  1. 如果你在实验的过程中发现HLFS driver for Openstack有问题，或者某些步骤不是很合理，存在问题，请及时指正。