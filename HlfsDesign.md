# hadoop dfs log structure filesystem design #
> -- 康华

## 背景介绍 ##
> HLFS的使用场景是为弹性云平台提供虚拟磁盘（类似amazon的EBS）后台存储服务。它需要满足高可用性、高性能、能随机读写、快速故障恢复、数据快照、回滚等特性。

## 实现简述 ##
> hadoop dfs 可被看做一个可靠的、随时可扩展的“磁盘”；但美中不足的是其不能随机写，只能追加写入，因此还不能直接作为我们的镜像存储系统。那么我们需要借助log structrue filesystem的理念，使用追加方式在hdfs上实现随机读写的要求。

## 特性描述 ##
  * 用户态文件系统     —— 与满足Unix可移植操作系统接口标准(Portable Operating System Interface of Unix, POSIX)的内核态文件系统不同，HLFS需要实现于用户态，以动态函数库的形式嵌入到客户端。HLFS不必完全实现POSIX所有的接口规范，只需实现虚拟机镜像存储所需要的pread、pwrite等主要接口即可。
  * 支持随机读写       —— HLFS文件要能支持随机读写，即可以按照偏移量(offset)定位读写位置。
  * 支持超大文件存储   —— 单个文件需可支持以TB为单位的数据存储（这里的文件大小对应的是虚拟机的磁盘大小）。
  * 仅支持单文件       —— 在HLFS的应用场景中，每个虚拟磁盘对应一个HLFS实例。因此，最简单的设计就是整个文件系统只需要支持一个文件，该文件对应虚拟机中的一个磁盘。
  * 面向客户端(Client-oriented)一致性 —— 虚拟机磁盘在同一时刻只能被挂载到一个虚拟机镜像中。因此，只需要对其挂载的虚拟机保证数据一致性即可。
  * 支持快照和回滚 —— HLFS文件可随时进行现场快照、可按需进行回滚。对于云计算平台所提供的虚拟机，快照回滚是防止网站被入侵或者误操作造成数据污染、损坏的最有效手段。
  * 支持数据多副本   —— 数据存储多份以保证数据安全性，并对数据访问进行就近访问优化(HDFS已经为我们实现这点)。
  * 集群系统可动态扩容 —— 存储节点（PC服务器）可动态添加到集群中，而不影响集群正常服务（hadoop dfs为我们实现这点）
  * 集群系统可透明故障切换 ——  存储节点出现故障，不影响服务，对用户透明（hadoop dfs为我们实现这点）

## 概要设计 ##
HLFS的磁盘数据格式与一般文件系统（FFS—fast filesystem）无多大差异，都是借助于indirect block 、inode 、directory 等结构。
所不同之处在于LFS会将磁盘（我们这里是hdfs的存储池）分割成有序的segment 进行管理，当前活跃的segment只有一个（也就是我们日志的逻辑尾的segment）。
这些segment 逻辑上头尾相连组成一个线性log，任何的文件更新（数据块、indirect block 、inode等等）都会以追加方式写入到log尾部——显然这么做的好处是保证了磁头的顺序移动，提高了吞吐；而带来的麻烦是需要回收前期写入的旧数据（修改过的），否则磁盘迟早会写满。综上所述我们设计的最基本思路是 —— 利用hadoop dfs为我们提供可靠的、分布式的存储介质；然后在其上实现LFS。
![http://cloudxy.googlecode.com/svn/wiki/images/HLFS.jpg](http://cloudxy.googlecode.com/svn/wiki/images/HLFS.jpg)

下面就关键设计技术点做概要设计。

### HLFS LOG 创建和检索 ###
> LOG 是我们数据持久化的一个基本写入单位,对于写透需求来说，实际上每次写入动作都会产生一个新的LOG，而每次的LOG大小不尽相同。
LOG的内容显然必须包含被写入的数据块，还需要包含对应的元数据（索引块等）信息，以及元数据的元信息（inode)，这样才能完成对一个信息的索引。

#### LOG创建 ####
> 任何文件或者目录的修改LFS都需要向log中写入如下几部分信息，而且要求严格“按照顺序写入（in-order semantics）”—— 其目的是为了崩溃时能尽可能恢复数据一致性。
  1. 任何修改或者新数据块  （数据块是数据实际操作的最小粒度；可配置；大小为8196字节或者更大）
  1. Indirect block 更新   （也叫做meta-data block ，是inode中的2/3/4级索引块数据）
  1. Inode          更新   （每个inode对应一个文件，如果只支持单一文件，则只用一个inode)
  1. Inode map      更新    ( 单一文件则只有一个映射项）
> LOG 的布局为 ： **A (data）| B (meta-data block log) | C (inode log) | D(inode\_map log)**。

![http://cloudxy.googlecode.com/svn/wiki/images/log-struct.png](http://cloudxy.googlecode.com/svn/wiki/images/log-struct.png)

#### 数据检索 ####
> 读取文件最新数据时：需要通过找到最新的inode map位置，再进而找到所需文件对应inode，再进而找到文件逻辑地址对应的数据块的物理地址（段号+offset)，再进而读取数据。
> 最新Inode map位置按理应记录在checkpoint文件中（见错误恢复部分），HDFS初始化加载时读入；如果运行中则该inode map 驻留于内存数据结构中。
  * 注意，文件块大小是可变的（可配置），比如8k。对于不足一个块的修改，一定会伴随先读出完整块再修改，再追加这一过程。
![http://cloudxy.googlecode.com/svn/wiki/images/block-address.png](http://cloudxy.googlecode.com/svn/wiki/images/block-address.png)
### 空间管理 - 段 ###
  * 分段 —— 为了空间管理和回收的目的，我们必须对存储空间进行分段：每个段大小并不需要固定，而且具体大小也没有硬性规定；
    * 每个HLFS的段被映射成一个Hadoop Dfs文件，比如大小为64M（可配置）；segment文件名字格式为segno.seg，其中segno从0开始递增；
    * 数据block的存储地址（storage address)为64位，前部为segment的文件号，尾部为segment文件内偏移量——比如一个段大小是64M，则尾部需要26位；
  * 段回收 ——（Segment Clean）log structure 文件系统顺序追加写行为必然带来大量的旧数据，因此必须有旧数据回收机制。回收机制大体来讲需要遍历段文件中的各数据块，检查当前inode的索引块中的地址是否有指向它，如果没有则说明是该数据块是旧数据可以放弃。如果一个段中的所有数据块都是旧数据块，则这个段文件可被回收（从hdfs中删除）。另外当段中可用数据块很少时，也可以采取copy and remove的方式——将非旧数据的数据块主动以log形式复制写到新段中，从而使得原段中的所有数据块都成为旧数据块，这时再删除其。
在hlfs的实现中，将段回收过程分成两部走：
    1. 第一步计算段的使用统计（统计段中当前还有多少数据块活跃）；
    1. 第二步对活跃块小于水平线的段（水平线可设置）执行删除或者copy and remove动作，来回收空间。
    * “段使用统计“由外部工具程序实现，产生的段使用计数存放到segment\_usage.txt中。我们可以从hlfs客户端所在的机器以"拉模式下"（将段读入到本地）执行段统计任务；也可以采用"推模式下"(map/reduce 方式就近计算）并行执行段统计任务。
    * “段回收”工作则由hlfs唯一的写入线程执行，从而避免元数据(inode)并发修改带来的问题，另外回收作为低优先级任务，会首先让路给正常的写入任务，只有在当前没有写入任务时才会开始执行段回收任务。

### 快照实现 ###
> 在log structure filesystem 上实现快照功能是先天具备的特性。
> 我们每次写入的log（凡是没有被回收的log)都可作为一个快照点，也就是说都可以恢复当时的数据，具体做法非常简单，只需要载入快照所在时刻那个log中的inode即可。所以原则上我们的数据可以回滚到任何一次写入时刻。不过，我们一般不会保留所有数据，因此建议用户为自己认为重要的数据状态做一个标记，这样就可以得到一个永久快照，这个永久快照将被写入到checkpoint.txt文件中（记录快照时刻的inode地址、时间、名称即可），这个快照所在inode的数据不被回收。

## HLFS系统需要的文件（都是追加生成） ##
  * SEGMENT 文件        ——  主要的数据存储体，具体见上文。
  * Superblock 文件     ——  记录文件系统的配置，如块大小，段大小，采用引擎（本地还是hadoop)等配置信息，这些信息一旦确定不再变化。具体见mkfs.hlfs。
  * Segments\_usage.txt 文件 ——  段记录使用统计文件，每行代表一个段。
  * snapshot.txt  文件    —— 记录特定时刻的系统数据版本，具体记录的是特定时刻inode的位置。

## 主要API ##
> api实现有几个特点：我们采用C风格api；线程安全（因为访问者各自维护HLFS\_CTRL上下文）;接口类似文件系统（但由于不存在多文件概念，因此不存在文件名之类概念）
  * HLFS\_CTRL\*init\_hlfs(const char\*uri)
  * int deinit\_hlfs(HLFS\_CTRL\*ctrl)
  * int hlfs\_stat  (HLFS\_CTRL\*ctrl,HLFS\_STAT\_T\*stat)
  * int hlfs\_open  (HLFS\_CTRL\*ctrl,int flag)
  * int hlfs\_close (HLFS\_CTRL\*ctrl)
  * int hlfs\_read  (HLFS\_CTRL\*ctrl,char\*read\_buf,uint32\_t read\_len,uint64\_t pos)
  * int hlfs\_write (HLFS\_CTRL\*ctrl,char\*write\_buf,uint32\_t write\_len,uint64\_t pos)

## 当前实现不尽人意的地方 ##
  1. 内存管理粗犷式-目前代码中内存使用没有精细化，很多地方用的比较浪费（甚至可能有少量内存泄露)。所以运行时，尽量内存宽松点。如果是dom0中运行可别把dom0配置512M这么小，最少给个1G吧。
  1. 连接管理未池化，有很多重复打开问题—— 这里的连接是指和后台存储（hdfs的连接句柄）。这种反复打开和关闭，在网络环境中很是耗时，我们下来会减少多余的连接开合动作。
  1. 对于clean段操作中，有可能发生要删除的段还被访问的情况，这时安全的方式是采用“引用技术保护”，我们现在采用简单粗暴的加锁方式：读写时都加锁，所以未有读写操作时才会删除。后期会找机会改进。
  1. 如果文件未被调用close，则在另外的连接上下文中不能看到文件存在，这个问题在系统崩溃后重新加载时会给我们带来问题（注意文件虽然从namespace中看不到，但hadoop fs -tail却能看到内容，因为从inputstream中可读取未提交给name node，却存在于data node的无数据）,由于libhdfs中还未有读取inputstream方便的接口，所以现在我们采用一个很山寨的方法hadoop fs cp 来恢复最后的未提交数据。

```
    特别感谢
    hlfs开发起源不能不提到淘宝的杨志峰和其团队几位未曾谋面的朋友，他们在阿里云曾借鉴log-structured file system公开论文开发过一个标准的log structrue filesystem （轩辕系统）原来设想用于虚拟机存储，但很可惜由于种种原因项目最终夭折，没有被最后采纳。
    我有幸当时看过他们的文档和代码，虽然当时我对log structure filesystem理解和虚拟机镜像存储需求理解都尚且幼稚，可隐约也觉得log structure filesytem 结合 hdfs这种只能追加的分布存储系统是虚拟机镜像存储的一个不错选择（因此我还多次请教志丰，向其学习log structure filesystem 实现，其精湛技术和专业素质让人敬佩）。
    随着我对log structure filesytem 和虚拟机镜像IO的进一步研究和领域，更重要的是有志丰等人的先期工作证明了这种方案的可行，让我相信针对虚拟机镜像存储特点实现一个log structrue block 镜像存储系统确实是值得一试的。因此针对虚拟机镜像访问特点，我开始着手实现一个针对块（block）的log structure block系统，也就是现在的hlfs —— 它相比标准log structure filesystem 而言会实现的更加简练（因为没有file层次结构），其上的快照、merge 方案等都应更简练和高效，优化余地更大。
    很抱歉，hlfs发布初期没有提及志丰他们。直到今天在征得了志峰和其团队同意后，特别感谢他们当年的开创性尝试。如果没有他们前瞻性工作，hlfs肯定会走很多弯路。
    刚向志丰要来了那几位素未平生的朋友的名字（如下），再次感谢他们，也希望他们能对cloudxy多提建议，多施与援手—— 他们是：
    * 杨志丰 ： yangzhifeng83@gmail.com
    * 董超   ： dongchao51@hotmail.com
    * 郑文静 ： zhengwenjingster@gmail.com
    * 邓岩   ： dengyan@act.buaa.edu.cn
```

关于log structure filesystem 最初论文，请参看download中资料

---

written by 康华
