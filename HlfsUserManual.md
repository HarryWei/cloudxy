# HLFS 用户手册 #

## 准备动作 ##
基本环境要求：
  * 32、64位Linux系统操作系统（我是在ubuntu10.4和Centos上进行开发的）
  * 2G  以上内存
### Hadoop 安装 ###
> > hlfs需要使用hadoop hdfs的append功能实现log追加操作，因此我们需要选择hadoop-0.20-append或者cloudera的cdh3u0/1/2/3版本（默认支持了append功能，不用再配置 dfs.support.append=true了，如果使用其他版本一定要打开该功能），关于hadoop append问题域的分析请见http://www.cloudera.com/blog/2009/07/file-appends-in-hdfs.<br>
<blockquote>我们建议使用cloudera的cdh3u0/1/2/3，安装方式参见  <a href='https://ccp.cloudera.com/display/CDHDOC/CDH3+Installation。'>https://ccp.cloudera.com/display/CDHDOC/CDH3+Installation。</a><br>
另外必须指出的是，因为我们需要hdfs的hflush接口，而cdh3的发布包里没有加入，因此手动打一个小patch(<a href='https://issues.apache.org/jira/secure/attachment/12482311/HDFS-2055.r2.patch);'>https://issues.apache.org/jira/secure/attachment/12482311/HDFS-2055.r2.patch);</a> 然后需要重新编译libhdfs(ant -Dlibhdfs=yes -Dcompile.c++=yes compile-c++-libhdfs）——为了方便大家，我们将编译后的libhdfs置于我们的第三方库目录下/3part/hadoop/，所以大家编译和运行时注意指向我们修改后的libhdfs!!!<br>
<h3>Xen 安装</h3>
我们使用xen中的tapdisk2模式加载hlfs磁盘，因此需要使用xen4.0之上版本（如果低版本xen需要打上tapdisk2的patch)。因此我们建议使用xen4.1.x 。 对于dom0内核版本，我们建议不要用2.6.18的老内核，使用当前2.6.32以上的内核比较可靠。<br>
关于xen的安装，网上资料很多。<br>
也可以参见我们wiki上的资料。如果启动虚拟机有问题，去看看权威文档，相信所有问题都有解决办法<a href='http://wiki.xen.org/xenwiki/XenCommonProblems。'>http://wiki.xen.org/xenwiki/XenCommonProblems。</a><br>
<pre><code>hlfs也支持qemu（从而支持kvm)，具体参见http://code.google.com/p/cloudxy/wiki/HLFS_SUPPORT_QEMU<br>
</code></pre></blockquote></li></ul>

<h2>HLFS START UP</h2>
<blockquote><h3>获取hlfs代码和相关依赖库</h3>
</blockquote><ul><li>SVN方式：在终端键入'svn checkout <a href='https://cloudxy.googlecode.com/svn/trunk/hlfs/'>https://cloudxy.googlecode.com/svn/trunk/hlfs/</a> hlfs' （因为需要包含了第三方库缘故，因此肯恩checkout时间比较长。如果想修改则请用https co）<br>
</li><li>Git方式：git clone git://github.com/kelvin-xupt/hlfs.git<br>
</li></ul><blockquote><h3>编译hlfs</h3>
<ol><li>安装cmake;安装glib库;安装java。<br>
<pre><code>  java 安装sun的<br>
  sudo apt-get install python-software-properties<br>
  sudo add-apt-repository ppa:sun-java-community-team/sun-java6<br>
  sudo apt-get update<br>
  sudo apt-get install sun-java6-jdk<br>
  或者<br>
  sudo add-apt-repository ppa:ferramroberto/java <br>
  sudo apt-get update<br>
  sudo apt-get install sun-java6-jdk<br>
</code></pre>
</li><li>修改在hlfs/src/CMakeLists.txt中配置你正确的java目录 ‘如 SET(JAVA_HOME   /usr/lib/jvm/java-6-sun)'。<br>
</li><li>生成makefile:cd hlfs/build；cmake ../src（生成makefile)<br>
</li><li>make {all | libhlfs | tools | test}<br>
<ul><li>all: 生成所有库和工具<br>
</li><li>libhlfs: 生成hlfs动态库<br>
</li><li>tools: 生成所有工具<br>
<pre><code>   1. src hlfs库源代码目录<br>
   2. nbd-2.9.15 修改后的nbd-server/nbd-client源代码目录<br>
   3. 3part 第三方库，目前包含hadoop相关;log库<br>
   4. output 编译后的生成库和工具所在目录<br>
   5. patches hlfs driver for blktap2的patch代码<br>
   注：有些linux版，可能会出现 “usr/include/glib-2.0/glib/gtypes.h:34:24: fatal error: glibconfig.h: No such file or directory” ，别慌，执行 sudo cp /usr/lib/i386-linux-gnu/glib-2.0/include/glibconfig.h /usr/include/glib-2.0/ 或者 执行sudo cp /usr/lib/x86_64-linux-gnu/glib-2.0/include/glibconfig.h /usr/include/glib-2.0/ 即可.<br>
</code></pre>
</li></ul></li></ol><h3>测试hlfs</h3>
为了方便调试目的，hlfs实现中支持了本地测试（不需要hadoop hdfs),我们称为local模式。我们首先用local模式测试<br>
<ul><li>编译hlfs - cd hlfs/build;cmake ../src/;make all<br>
</li><li>local模式下格式化hlfs文件系统<br>
<ol><li>mkdir /tmp/testenv //需要一个类似workshop的工作环境<br>
</li><li>./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024 (-b是数据块大小，单位是byte， -s是段文件大小，单位是byte， -m 是虚拟磁盘的最大容量,单位是M)<br>
</li></ol></li><li>编译测试用例 - mkdir hlfs/test/build && cd hlfs/test/build && cmake .. && make all<br>
</li><li>执行测试用例 - ./outpub/bin/test -u local:///tmp/testenv/testfs  -r 4096 -a 409600(向uri所指向的fs中尾部开始每次追加式写入4096字节，共写入409600字节；然后再从头开始每次读取4096字节，共读取409600字节）<br>
<pre><code>    * 格式化目的是写入原数据到superblock中，这些元数据描述了该hlfs的属性信息；当格式化完成后，你会在/tmp/testenv目录下，发现testfs目录，再其下发现superblock文件，查看该文件，会发现块大小、段大小、容量限制等该hlfs的属性信息<br>
    * 格式化时你可创建比你实际拥有的存储更大的空间，比如你所有存储当前只有500G,但你可以创建1T的虚拟盘，待你真的存储需求接近500G时，再扩容存储不迟（这就是当今所谓的Thin Provisioning概念）<br>
    * 执行完毕后，会在/tmp/testenv/testfs下发现生成的段文件<br>
</code></pre>
</li></ul>如果你已经安装hadoop hdfs，并且已经启动，则可以测试hdfs模式，测试方式<br>
<ul><li>编译hlfs - cd hlfs/build;cmake ../src/;make all<br>
</li><li>hdfs 模式下格式化hlfs文件系统 (我们以伪分布模式执行）<br>
<ol><li>hadoop fs -mkdir hdfs:///tmp/testenv<br>
</li><li>./output/bin/mkfs.hlfs -u hdfs:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024<br>
</li></ol></li><li>编译测试用例 - mkdir hlfs/test/build && cd hlfs/test/build && cmake .. && make all<br>
</li><li>执行测试用例 - ./test -u hdfs:///tmp/testenv/testfs -r 4096 -a 409600<br>
<pre><code>  * 可见local模式和hdfs模式只是uri的head不同而已，其余用法类似<br>
  * hdfs模式uri支持<br>
     1. hdfs:///tmp/testenv/testfs                       //hdfs伪分布模式下使用<br>
     2. hdfs://192.168.0.1/tmp/testenv/testfs            //默认端口为8020<br>
     3. hdfs://192.168.0.1:8020/tmp/testenv/testfs       //完整uri；192.168.0.1也可是主机名代替<br>
  * 执行hadoop fs -ls /tmp/testenv/testfs命令你可看到类似local模式下的各种文件(hadoop shell 命令请见http://hadoop.apache.org/common/docs/r0.17.2/hdfs_user_guide.html)   <br>
  * 说明一下hadoop/libxx/libhdfs.so库并非标准ch3代码编译而得，因为标准代码没有支持hflush操作，为了支持该操作，我再ch3源代码上打了hflush补丁（patch请见https://issues.apache.org/jira/browse/HDFS-2055)。 <br>
  * 按照hdfs模式执行hlfs必须配置classpath — 如我机器配置如下：CLASSPATH=/usr/lib/jvm/java-6-sun/lib/dt.jar:/usr/lib/jvm/java-6-sun/lib/tools.jar:/usr/lib/hadoop-0.20/conf:/usr/lib/jvm/java-6-sun/lib/tools.jar:/usr/lib/hadoop-0.20:/usr/lib/hadoop-0.20/hadoop-core-0.20.2-cdh3u1.jar:/usr/lib/hadoop-0.20/lib/ant-contrib-1.0b3.jar:/usr/lib/hadoop-0.20/lib/aspectjrt-1.6.5.jar:/usr/lib/hadoop-0.20/lib/aspectjtools-1.6.5.jar:/usr/lib/hadoop-0.20/lib/commons-cli-1.2.jar:/usr/lib/hadoop-0.20/lib/commons-codec-1.4.jar:/usr/lib/hadoop-0.20/lib/commons-daemon-1.0.1.jar:/usr/lib/hadoop-0.20/lib/commons-el-1.0.jar:/usr/lib/hadoop-0.20/lib/commons-httpclient-3.0.1.jar:/usr/lib/hadoop-0.20/lib/commons-logging-1.0.4.jar:/usr/lib/hadoop-0.20/lib/commons-logging-api-1.0.4.jar:/usr/lib/hadoop-0.20/lib/commons-net-1.4.1.jar:/usr/lib/hadoop-0.20/lib/core-3.1.1.jar:/usr/lib/hadoop-0.20/lib/hadoop-fairscheduler-0.20.2-cdh3u1.jar:/usr/lib/hadoop-0.20/lib/hsqldb-1.8.0.10.jar:/usr/lib/hadoop-0.20/lib/jackson-core-asl-1.5.2.jar:/usr/lib/hadoop-0.20/lib/jackson-mapper-asl-1.5.2.jar:/usr/lib/hadoop-0.20/lib/jasper-compiler-5.5.12.jar:/usr/lib/hadoop-0.20/lib/jasper-runtime-5.5.12.jar:/usr/lib/hadoop-0.20/lib/jets3t-0.6.1.jar:/usr/lib/hadoop-0.20/lib/jetty-6.1.26.jar:/usr/lib/hadoop-0.20/lib/jetty-servlet-tester-6.1.26.jar:/usr/lib/hadoop-0.20/lib/jetty-util-6.1.26.jar:/usr/lib/hadoop-0.20/lib/jsch-0.1.42.jar:/usr/lib/hadoop-0.20/lib/junit-4.5.jar:/usr/lib/hadoop-0.20/lib/kfs-0.2.2.jar:/usr/lib/hadoop-0.20/lib/log4j-1.2.15.jar:/usr/lib/hadoop-0.20/lib/mockito-all-1.8.2.jar:/usr/lib/hadoop-0.20/lib/oro-2.0.8.jar:/usr/lib/hadoop-0.20/lib/servlet-api-2.5-20081211.jar:/usr/lib/hadoop-0.20/lib/servlet-api-2.5-6.1.14.jar:/usr/lib/hadoop-0.20/lib/slf4j-api-1.4.3.jar:/usr/lib/hadoop-0.20/lib/slf4j-log4j12-1.4.3.jar:/usr/lib/hadoop-0.20/lib/xmlenc-0.52.jar:/usr/lib/hadoop-0.20/lib/jsp-2.1/jsp-2.1.jar:/usr/lib/hadoop-0.20/lib/jsp-2.1/jsp-api-2.1.jar<br>
</code></pre></li></ul></blockquote>

<h2>HLFS 存储虚拟机镜像</h2>
如果上述步骤成功，那么可以开始尝试EBS了。使用HLFS作为虚拟机的数据盘，甚至可以做系统安装盘。<br>
<ul><li>添加hlfs driver for blktap2<br>
<ol><li>进入xen/tools目录<br>
</li><li>打上patches目录下的hlfs_driver_for_blktap2.patch - 在xen/tools目录下执行patch -p1 < patches/hlfs_driver_for_blktap2.patch （该patch针对xen4.1.1版本，其他版本请自己修改啦;另外要记得tools/blktap2/drivers/Makefile中请修改HLFS_DIR变量指向svn co出来的hlfs目录，才能正确找到对应库和头文件）<br>
</li></ol></li><li>编译blktap2，安装blktap2，激活blktap2<br>
<ol><li>在tools目录下执行make clean;make;make install<br>
</li><li>重启动xend - service xend restart<br>
</li></ol></li><li>测试hlfs driver for blktap2<br>
<ol><li>local模式测试- mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024；tap-ctl create -a hlfs:local:///tmp/testenv/testfs；<br>
</li><li>hdfs模式测试-  mkfs.hlfs -u hdfs:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024；tap-ctl create -a hlfs:hdfs:///tmp/testenv/testfs<br>
</li></ol></li><li>作为DomU的数据盘使用-EBS用法。<br>
<ol><li>可以使用动态挂载 如 xm block-attach <br>
<br>
<domain><br>
<br>
 tap2:hlfs:local:///tmp/testenv/testfs xvdb w<br>
</li><li>通过配置文件配置 如 disk = ['tap2:aio:root/tapdisk-test/centos-mother.img,xvda,w','tap2:hlfs:local:///tmp/testenv/testfs,xvdb,w']<br>
</li></ol></li><li>作为DomU的系统盘使用<br>
<ol><li>直接使用生成的tapdev设备（如/dev/xen/blktap-2/tapdev0），做类似phy磁盘的使用。<br>
</li><li>使用生成的tapdev设备，格式化后，存储vhd镜像文件，再使用vhd镜像文件启动。<br>
<pre><code>     * tap-ctl和xm或xl等虚拟机管理命令，往往需要root权限执行，当你使用sudo 或者切换root用户执行时，别忘记了必须有正确的classpath! <br>
     * 如果xm cr 启动虚拟机，你可能会发现虚拟磁盘挂在失败！ 原因还是环境变量问题。—— xm cr &lt;xxx&gt;命令实际上是委托xend程序去启动(以子进程方式）tap-ctl；近而tap-ctrl又启动tapdisk2（同样子进程方式）、而tapdisk2会最终去执行hlfs_init（对hlfs发起connect）。我们知道子进程会自动继承父进程的环境变量，所以问题归咎为xend的环境变量到底是什么呢？（cat /proc/&lt;xend's pid&gt;/environ可以看到xen当前的环境变量）。 如果你的xend是系统启动完毕，且在root环境下设置CLASSPATH，再启动的，那你的CLASSPATH一定OK;但如果xend作为系统服务，当系统boot-up时候会早早的自动启动的（这时还不会去读取/etc/profile ~/.bashrc等文件），如果这样的话，你只有到/etc/init.d/xend脚本里自己手动在加入“export CLASSPATH=***"了！（记得加在xend start之前）<br>
</code></pre>
<h2>HLFS 用作虚拟网盘</h2>
</li></ol></li></ul><ul><li>编译nbd<br>
<ol><li>cd nbd-2.9.15/build;<br>
</li><li>cmake ../;<br>
</li><li>make (编译后内容在nbd-2.9.15/output下）<br>
</li></ol></li><li>加载模块 - sudo modprobe nbd (nbd服务需要一个对应的内核模块，这个模块一般没有被默认加载）<br>
</li><li>启动nbd server - ./nbd-2.9.15/output/bin/nbd-server 20000 (20000是服务端口号）<br>
</li><li>格式化hlfs - 如上所示 （如 mkfs.hlfs -u hdfs:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024；mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024)<br>
</li><li>挂接nbd设备 - 将nbd设备和上面格式化的fs关联（如./nbd-2.9.15/output/bin/nbd-client bs=512 127.0.0.1 20000 local:///tmp/testenv/testfs /dev/nbd0;）<br>
</li><li>测试nbd设备<br>
<ol><li>sudo mkfs /dev/nbd0<br>
</li><li>sudo mount /dev/nbd0 /opt<br>
</li><li>do everything like a local device</li></ol></li></ul>

<pre><code>我们对传统nbd server和 nbd client做了一些修改：<br>
 1. nbd-server 只需要给出端口号，不用给出其他信息了<br>
 2. nbd-client 要给出对应fs的uri地址——当和server握手时传给对方，server 再根据该uri初始化hlfs上下文，然后通过hlfs_stat获得fs大小信息，并传给client端<br>
 3. 我们还额外实现了nbd-ops工具用于控制hlfs的merge行为（具体见高级用法），外部控制采用管道通讯+共享内存实现，具体请见代码<br>
    <br>
   另外nbd还有一个讨厌的问题 —— nbd 死锁问题值得注意 :<br>
   nbd-server 如果和nbd-client如果部署在同一个机器上，在某些内核上会出现IO hang住（死锁）的问题。追其原因是：当系统内存不足时，必然需要pdflush例程将脏数据刷到磁盘已回收可用内存，而引发内存分配的肇事进程则必须同步等待上述释放动作，然后再获取刚回收来的内存继续运行。<br>
   那么可以想象当nbd-server运行时需要内存，但系统空闲内存不足时，就会触发pdflush的刷新操作，这是要被写入脏页的磁盘又是自己时（因为nbd盘作为数据盘也要被写入脏页的），则会出现死锁——因为要靠nbd-server继续运作才能将脏页写下去，而nbd-server运作的前提又是需要获得内存。<br>
   不过并非所有内核都会有这个问题（2.6.18肯定是有,新内核上后来引入pdi概念、pdflush刷新方式改变等使得死锁问题得以消除），我使用的ubuntu 2.6.35的内核就没有此问题。因此如果要在本地尝试nbd，最好使用高版本内核，或者内存给的大大的。实在不行，就麻烦你就去修改一下内核了：没记错的话，大概是在bdi_congestion处做一下手脚，让其避开nbd-server这个进程好了:)<br>
</code></pre>


<h2>旧数据merge功能</h2>
从设计文档中可以看到hlfs需要使用merge功能消除旧数据。而我们将merge过程分为俩步走：<br>
<ol><li>段统计：计算当前时刻的段活跃数据块数量，为下一阶段回收做一定预先判断（注意只是预判断，当回收任务执行时，段中活跃数据块数据有可以变化）。<br>
</li><li>回收段：对于那些活跃数据块小于给定阀值，但尚未为0的段，搬迁其依旧活跃的数据段（相对回收当前时间）到新段，然后在回收该段；如果活跃数已经为0，则直接回收该段。<br>
<h3>段统计</h3>
</li><li>采用拉模式：即在通过本地客户端方式执行 — segcalc.hlfs -u uri （在uri所在目录升到成段统计文件）<br>
</li><li>采用推模式: 即采用map/reduce模式，将该任务下发到段所在机器执行，然后将段统计值用reduce方式写到段统计文件中。具体做法如下：<br>
<ol><li>编译map/reduce执行程序 - cd hlfs/src/clean/Mapreducer/build;cmake ..;make<br>
</li><li>将上述执行程序上传到hfds下指定目录 - hadoop fs -put hlfs/src/clean/Mapreducer/outpub/bin/mr_segcalc.hlfs /exe/<br>
</li><li>下发map/reduce job - hadoop pipes -D mapred.reduce.tasks=1 -input /tmp/testenv/testfs  -output /output -program /exe/mr_cleaner<br>
<pre><code>采用推模式map/reduce实现段统计可发挥分布系统并行处理优势，但会带来额外的网络负担。<br>
关于C++ pipe实现map reduce程序请见pipe的例子代码。还有http://wiki.apache.org/hadoop/C%2B%2BWordCount#Upload_C.2B-.2B-_binary_files_to_HDFS等资料。<br>
</code></pre></li></ol></li></ol>

<h3>段回收</h3>
<ul><li>离线情况下，standalone方式运行段回收<br>
<blockquote>seg.clean -u uri -s segno -w waterlevel （如果segno是-1， 那么段清理将从第一个段开始，依次往后进行清理工作；waterlevel是回收阀值，单位是数据块，如果段中活跃数据块小于waterlevel则，执行清理工作）</blockquote></li></ul>

<ul><li>在线情况下，运行段回收。<br>
<blockquote>在线情况下，不能使用上述命令直接跑段回收任务，否则会造成数据破坏。在线情况下，段回收目前设计在hlfs的写入线程中实现。段回收任务相比写入请求而言，属于低优先级，因此只有当不再有写入请求的情况下，才会开始执行。<br>
</blockquote></li></ul><ol><li>tapdisk2 开启段回收<br>
<blockquote>outpub/bin/ -p pid -u uri -c cmd -r parameter -v verbose (-p后跟正在运行的nbd主进程号， -u后跟文件系统路径，-c后跟要执行的命令，包括start_merge：段回收开启；stop_merge：段回收停止；set_copy_waterlevel：设置回收阀值；query_stat：查询状态,r后面跟着相应命令所对应的参数，-v是是否输出详细的运行信息)。<br>
</blockquote></li><li>nbd-server 开启段回收<br>
<blockquote>outpub/bin/tapdisk_ops -f fsname -c cmd -r parameter -v verbose (-f后跟着文件系统名称，-c后跟要执行的命令，包括start_merge：段回收开启；stop_merge：段回收停止；set_copy_waterlevel：设置回收阀值；query_stat：查询状态, -r后面跟着相应命令所对应的参数，-v是是否输出详细的运行信息)。<br>
<pre><code>   对于hlfs库来说，<br>
   1.hlfs_clean_start(HLFS_CTRL *ctrl);<br>
   2.hlfs_clean_stop(HLFS_CTRL *ctrl);<br>
   3.int  hlfs_set_clean_level(HLFS_CTRL *ctrl,unsigned int alive_bytes);<br>
   等几个函数对应了上述命令的最终调用。<br>
   nbd-server和tapdisk2的外部控制命令实现不尽相同，tapdisk2用的更为简单，nbd-server实现复杂些，但更灵活。因此两种方式都保留，供大家参考。<br>
</code></pre></blockquote></li></ol>

<h3>关于日志</h3>
hlfs支持log4c日志系统，但作为库，日志系统的初始化工作并不会去做。如果需要使用请在调用方执行log4c_init()<br>
<br>
<h3>更新</h3>
# 目前由于支持块内压缩功能,所以mkfs命令增加了参数-c 0/1；0代表不压缩，1代表压缩。其余结构不变。<br>
<pre><code>./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024 -c 1<br>
</code></pre>



<h3>关于bug</h3>
hlfs完全由兴趣而起，业余时间开发，时间和精力都无法完全投入，所以内部难免很多功能测试不完善，留有不少bug，因此请见谅。<br>
如果您有兴趣试试，发现bug,欢迎提给kanghua151@msn.com 或者 cloudxy@googlegroups.com(我们的邮件列表），更欢迎直接修改ci。<br>
谢谢<br>
<hr />
written by 康华<br>
