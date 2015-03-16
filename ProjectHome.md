# CLOUDXY立足于实现网络虚拟存储的弹性云计算平台 #
> ![http://cloudxy.googlecode.com/svn/wiki/images/bitcoin.png](http://cloudxy.googlecode.com/svn/wiki/images/bitcoin.png)  _该项目乐意接受比特币(BitCoin)形式捐助 <捐助地址:**16r3dFo9f1WzuvV6EmBQTcmuqNgvcTHP7Q**>_
该项目主要包含有两个子项目：
  * HLFS - 虚拟机分布式镜像存储 (类似于亚马逊EBS，首先发布出来）
  * ECMS - 虚拟环境管理系统 （暂停——因为openstack等项目的蓬勃发展，已经不不要再重复造轮子了）

> ![http://cloudxy.googlecode.com/svn/wiki/images/arch.png](http://cloudxy.googlecode.com/svn/wiki/images/arch.png)

> How-To-Join-Us            — http://code.google.com/p/cloudxy/wiki/HlfsJoinUs

---

> > HLFS资料请参见
    1. FAQ                — http://code.google.com/p/cloudxy/wiki/HlfsFAQ
    1. High Level Design  — http://code.google.com/p/cloudxy/wiki/HlfsDesign
    1. Snapshot Design    — http://code.google.com/p/cloudxy/wiki/HlfsSnapshotDesign
    1. Cache Design       — http://code.google.com/p/cloudxy/wiki/HLFS_CACHE_DESIGN
    1. Segment Recycle    — http://code.google.com/p/cloudxy/wiki/SEGMENT_RECYCLE_WITH_SNAPSHOT_DESIGN
    1. 优化文档           — http://code.google.com/p/cloudxy/wiki/Optimization_For_Performance
    1. User Manual        — http://code.google.com/p/cloudxy/wiki/HlfsUserManual
    1. Source Code        — http://cloudxy.googlecode.com/svn/trunk/hlfs/
    1. Dev Road Map       — http://code.google.com/p/cloudxy/wiki/Hlfs_Development_RoadMap
```
    特别感谢
    hlfs开发起源不能不提到淘宝的杨志峰和其团队几位未曾谋面的朋友，他们在阿里云曾借鉴log-structured file system公开论文开发过一个标准的log structrue filesystem （轩辕系统）原来设想用于虚拟机存储，但很可惜由于种种原因项目最终夭折，没有被最后采纳。
    我有幸当时看过他们的文档和代码，虽然当时我对log structure filesystem理解和虚拟机镜像存储需求理解都尚且幼稚，可隐约也觉得log structure filesytem 结合 hdfs这种只能追加的分布存储系统是虚拟机镜像存储的一个不错选择（因此我还多次请教志丰，向其学习log structure filesystem 实现，其精湛技术和专业素质让人敬佩）。
    随着我对log structure filesytem 和虚拟机镜像IO的进一步研究和领域，更重要的是有志丰等人的先期工作证明了这种方案的可行，让我相信针对虚拟机镜像存储特点实现一个log structrue block 镜像存储系统确实是值得一试的。因此针对虚拟机镜像访问特点，我开始着手实现一个针对块（block）的log structure block系统，也就是现在的hlfs —— 它相比标准log structure filesystem 而言会实现的更加简练（因为没有file层次结构），其上的快照、merge 方案等都应更简练和高效，优化余地更大。
    很抱歉，hlfs发布初期没有提及志丰他们。直到今天在征得了志峰和其团队同意后，特别感谢他们当年的开创性尝试。如果没有他们前瞻性工作，hlfs肯定会走很多弯路。
    刚向志丰要来了那几位素未平生的朋友的名字（如下），再次感谢他们，也希望他们能对cloudxy多提建议，多施与援手—— 他们是：
    杨志丰 yangzhifeng83@gmail.com;董超 dongchao51@hotmail.com;郑文静 zhengwenjingster@gmail.com;邓岩 dengyan@act.buaa.edu.cn
```

---

> > ECMS 资料参见
    1. ECMS High Level Design —  http://code.google.com/p/cloudxy/wiki/ECMS_HIGH_LEVEL_DESIGN
    1. ECMS detail Desgin     — http://code.google.com/p/cloudxy/wiki/ECMS_Detail_Design
    1. ECMS Dev & Dbg Manual  — http://code.google.com/p/cloudxy/wiki/EMCS_RD_Manual
    1. HLFS Usage Demo        — http://code.google.com/p/cloudxy/wiki/HLFS_Used_In_VM
    1. Dev Road Map           — http://code.google.com/p/cloudxy/wiki/EMCS_Development_RoadMap

---

> > NEWS
    * 2011.8.1   ::HLFS 开始构思和调研
    * 2012.1.2   ::Snapshot basic version merge to trunk — snapshot basic version 支持了基本的线性快照.具体api请见http://code.google.com/p/cloudxy/wiki/HlfsSnapshotDesign
    * 2012.1.24  ::ECMS实现框架开发基本完成,下一步将实现各项命令.欢迎对erlang开发和分布系统有兴趣的朋友加入开发—框架代码请见https://cloudxy.googlecode.com/svn/trunk/ECMS
    * 2012.2.2   ::Snapshot Tree Structure version merge to trunk  — tree structure snapshot 支持了树型快照。这绝对是一个很酷的feature !
    * 2012.2.3   ::发布文章《谁来拯救云计算》 见http://blog.csdn.net/kanghua/article/details/7232191 ——文中就云计算的基础技术架构进行探讨。欢迎从事或对云计算有兴趣的朋友，参与讨论和批判!
    * 2012.2.10  :: 发布一个最小PV domU-x86供我们项目测试使用 —— 见http://code.google.com/p/cloudxy/wiki/HowTo_Build_PV_domU ; http://cloudxy.googlecode.com/files/domU-32bit-FS.img.tgz
    * 2012.2.14  :: 发布一个最小PV domU-x86\_64供我们项目测试使用 —— 见http://cloudxy.googlecode.com/files/domU-x86_64-FS.img2.zip ；另外又补充了带xenstore的镜像domU-64bit-withxenstore-FS.img.tar.gz
    * 2012.2.15  :: 引入www.do.com online项目管理，并邀请group中朋友加入
    * 2012.2.16  :: 补充了http://code.google.com/p/cloudxy/wiki/My_Test_Env_Build 一文，以自己环境为例，说明最简单的环境搭建和实验步骤
    * 2012.2.17  :: 补充了http://code.google.com/p/cloudxy/wiki/HowTo_Build_Xen_Env 一文，以自己环境为例，详细说明如何搭建Xen实验环境，给出了具体实验步骤
    * 2012.3.3   :: 整理了近期HLFS和EMCS的开发roadmap，欢迎感兴趣的朋友加入新功能的开发
    * 2012.3.19  :: 简单实现了一组HLFS在虚拟机中应用demo （ http://code.google.com/p/cloudxy/wiki/HLFS_Used_In_VM ）  系统能通过demo,提高大家对hlfs的认知程度。
    * 2012.3.25  :: HLFS Cache设计草案完成http://code.google.com/p/cloudxy/wiki/HLFS_CACHE_DESIGN 进入开发阶段。
    * 2012.4.7   :: HLFS Segment Recycle with snapshot 设计草案完成  http://code.google.com/p/cloudxy/wiki/SEGMENT_RECYCLE_WITH_SNAPSHOT_DESIGN 进入开发阶段。
    * 2012.5.2   :: HLFS Cache 开发完成,并已经合并到trunk，欢迎大家测试(可参见http://cloudxy.googlecode.com/svn/trunk/hlfs/test/test_base_with_cache.c 以及配置文件 http://cloudxy.googlecode.com/svn/trunk/hlfs/output/conf/hlfs.conf )
    * 2012.5.7   :: HLFS Segment Recycle with snapshot 开发完成，并合并到trunk,欢迎大家测试。可参见 http://code.google.com/p/cloudxy/wiki/SEG_CLEAN_USAGE ）
    * 2012.5.11  :: Performance tuning - indirect block read cache 开发完成，并合并到trunk,非常欢迎大家测试。具体代码见icache目录和logger目录下的代码。
    * 2012.5.14  :: Performance tuning - “段句柄操作池化” 和“锁优化” 完成，并合并到trunk,非常欢迎大家测试。
    * 2012.5.15  :: Performance tuning -  memory usage 优化完成，并合并到trunk,非常欢迎大家测试。
    * 2012.5.19  :: Performance tuning -  本阶段调优告一段落。
    * 2012.5.29  :: HLFS支持qemu（从而支持KVM) - 详情见http://code.google.com/p/cloudxy/wiki/HLFS_SUPPORT_QEMU
    * 2012.6.1   :: Clone功能实现，且集成到qemu中了- 详情见http://code.google.com/p/cloudxy/wiki/HLFS_SUPPORT_QEMU
    * 2012.6.6   :: AMD公司慧眼识珠，为我们了提供开发测试服务器。
    * 2012.7.7   :: HLFS 支持块内压缩功能完成(携子发布) （压缩目的是减少传输量，提高系统吞吐 http://code.google.com/p/cloudxy/wiki/About_block_Compression ),用法见http://code.google.com/p/cloudxy/wiki/HlfsUserManual?ts=1341649851&updated=HlfsUserManual 中更新一节。
    * 2012.12.1  :: HLFS 初步支持iSCSI（原型开发完毕）http://code.google.com/p/cloudxy/wiki/Support_iSCSI
    * 2013.2.24  :: HLFS 初步与OPENSTACK集成http://code.google.com/p/cloudxy/wiki/INTEGRATE_HLFS_INTO_OPENSTACK
    * 2013.3.15  :: HLFS 多文件扩展DEMO设计实现http://code.google.com/p/cloudxy/wiki/HlfsMultifileDesign

On the Way:
  * 支持预分配段文件 （目的是避免实时分配段文件的延迟 http://code.google.com/p/cloudxy/wiki/About_SegFile_Handle ）
  * 支持段句柄池化（目的是减少打开和关闭段文件操作，减少延迟 http://code.google.com/p/cloudxy/wiki/About_SegFile_Handle ）
  * 段文件启发式分目录存放（段文件过多时，需要分目录存放，已提高检索速度 http://code.google.com/p/cloudxy/wiki/About_SegFile_Handle ）
  * 做为存储backend，向openstack集成 ！！！！（完成）
  * 性能profiling
  * 段回收详细测试




