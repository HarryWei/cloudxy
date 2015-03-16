# Introduction #
svn checkout http://cloudxy.googlecode.com/svn/branches/hlfs/person/zhangdianbo/scripts scripts;cd scripts;./hlfs.sh

2013-03-02
升级说明：

---

1.改进了需要频繁输入sudo 密码问题。
2.添加对ld.so.conf的配置来解决发现库的问题。
3.添加命令 chmod +x /usr/local/lib/hlfs/bin/**，并添加到PATH
分别新增了三个函数，修改了一个函数，来处理添加PATH，处理一般profile添加，处理ld.so.conf的三个函数，另外把原来函数中的检测部分提出来单独作为一个工具使用。
function search\_key\_value\_in\_file()
function update\_profile()
function update\_ldconf()
function update\_path()
4.添加了apt sources 探测 和assert\_apt\_get\_update函数
5.添加了系统日志。Exit\_on\_Failure 会经常出现，没错只有on Failure 才会 Exit，所以你见到的大多数是绿色的，如果你见到红色的，那就是真的Exit了。
6.将原来的一个脚本分裂成若干不同功能的子脚本，添加setup、test等目录。
7.为hlfs.sh 提供可选参数。
8.修改了启动hdfs服务器的方法,避免了不必要的服务重启。
9.修改了function dealwith\_source\_or\_Exit()，函数配合不同的参数选项及本本机备份源程序的环境实施自动选择及备份。
10.添加了多核编译支持，提高编译速度。
11.修正了判断是否为已经打了patch的源码。

---

详细参数说明
./hlfs.sh -n**

> -d, --dest-dir
> > Specify the dest workspace directory, in witch source code will
> > download and build, if you would not specify it, where
> > cloudxy will be used as default workspace.


> -h, --source-hlfs
> > Provide local directory, if you have svn checkout it from
> > https://cloudxy.googlecode.com/svn/trunk/hlfs， hlfs.sh
> > would not destory it, only copy deeply it.
> > otherwise, hlfs.sh will checkout it a brand new


> -q, --source-qemu
> > Provide the faster sources or local place on which your have
> > git clone, otherwise,hlfs.sh will git clone it from the offical
> > site  git://git.qemu.org/qemu.git


> -l, --source-libvirt
> > And the like two of above, git clone libvirt is a long time
> > in GFW China Side, fortunately, hlfs.sh will git clone it for
> > you and keep it clean in workspace/backup with hlfs and qemu,
> > default .


> -p, --probe
> > Thanks for my lovely and Powerful GOV, it make the new Berlin
> > Wall, Thanks for the Roaring Beast Fang, if you Encounter the
> > same net problem, such as sudo apt-get update failure, can't
> > install package, try it. I do think you need this, because I
> > encounter with the hell hundreds.

./hlfs.sh -d ../cloudxy/ -h ../backup/hlfs/ -q ../backup/qemu/ -l ../backup/libvirt/

如果你因为某些软件包没有安装成功而退出，比如说build-essential，oracle-java7-installer不能成功安装，↑ blank -p Enter

./hlfs.sh -d ../cloudxy/ -h ../backup/hlfs/ -q ../backup/qemu/ -l ../backup/libvirt/ -p

遗留问题：
1.反复重新启动。
2.处理kev-value检查的时候尤其是hadoop classpath的时候风扇响的太可怕了，严重干扰老婆睡觉。
3.日志系统不理想。
4.测试功能没有集成。





2013-02-20
由于hlfs 涉及的第三方产品众多，安装和配置成为阻碍新用户的一道拦路虎，在康华的安装说明的基础与技术指导下，完成了这个一键安装的初级版本。下载并执行 chmod +x hlfs\_setup.sh; ./hlfs.sh
https://cloudxy.googlecode.com/svn/branches/hlfs/person/zhangdianbo/hlfs_setup.sh
执行后，会在hlfs\_setup.sh 所在目录下下载安装文件及源码。

目前运行环境 ubuntu server 12.04.1 ，我只在64位上测试，理论上可以在32位上使用。

目前已知问题有：
1.需要经常输入sudo password。
2.在desktop上安装 /etc/profile 中 LD\_LIBRARY\_PATH 无法被导出，但是server版本就没i有此问题，

JAVA\_HOME=/usr/lib/jvm/java-7-oracle
export JAVA\_HOME

LD\_LIBRARY\_PATH=$JAVA\_HOME/jre/lib/amd64/server
LD\_LIBRARY\_PATH=/usr/local/lib/hlfs/lib:$LD\_LIBRARY\_PATH
export LD\_LIBRARY\_PATH

3.如果中间因外部原因中断，通常来说重新运行./hlfs\_setup.sh

下一次更新会解决这个问题。

如果在使用中发现任何问题，请给我邮件（littlesmartsmart@gmail.com）或发issue