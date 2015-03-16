# Hadoop DFS log structure filesystem design #
> -- Written by Kang Hua <kanghua151@gmail.com>, Translated by Harry Wei <harryxiyou@gmail.com>.

## Background introduction ##
> HLFS(Hadoop DFS Log FileSystem) is used to provide virtual disk for elastic cloud platform (The same as Amazon's EBS) and backend storage service. It needs to satisfy some characters including usability, high-performance, random reading and writing, fast fault recovery, data snapshots, rollback and so on.

## Realization description ##
> Hadoop DFS can be regarded as a reliable, extensible 'disk' at any time. However, it cannot be writtern randomly but append sequentially. So we cannot take it as our mirror storage system directly. Thus we should realize randomly W/R operations by appending logs with the help of thoughts about 'log structure filesystem'.

## Descriptions Of Characteristics ##
  * User mode file system -- Be different from the file-systems of standard kernel mode posix interfaces. HLFS is realized at user mode space and inserted into client-side as lib mode. It is unnecessary to achieve posix interface regulations, just some main semantics like pread/pwrite, etc.
  * Support randomly reading and writing -- File should support randomly reading and writing (according to file offset's location).
  * Support large file storage -- Single file needs to support over Tera Bits storage (The file here means the size disk of virtual machine).
  * Only support single file -- Under our usage condition, every virtual disk is an HLFS case, so the easiest design is we only provide one file (a disk in the virtual machine).
  * Client-orient consistency -- Virtual machine disk can only be attached to one virtual machine mirror at the moment, so we only need provide data consistency of virtual machine client.
  * Support snapshots and rollback -- Files can be taken snapshots freely and rollbacked in need(To host service virtual machine, snapshot and rollback is the most effective way to protect datas from pollution and damage).
  * Support multidata and multicopy -- More than one storage can ensure data safety and nearby access (Hadoop DFS helps us realize this).
  * Cluster system can be dynamically expanded  -- Storage nodes (PC servers) can be dynamically added without affecting normal service of cluster (Hadoop DFS helps us realize this).
  * Cluster system can do fault switch implicitly  -- Faults of storage nodes do not affect service, and it is implicit to clients (Hadoop  DFS helps us realize this).


## Architectural Design ##
HLFS disk data format is almost the same as normal file system (FFS-fast file system ), both are with the structures like indirect block, inode, directory ,etc. The differences are HLFS can separate disk(Here is Hadoop DFS's storage pool) into segments to manage, and current time has only one active segment(That is our segment at the logic end). These segments is connected with head to tail and make up a linear log. And any file's updating(data block, indirect block, inode, etc) will add a log to the tail of current segment. Obviously，the advantage of this action ensure the magnetic head's orderly movement and enhance the throughputs. The trouble is we have to recycle old datas(have been changed).If not, the disk will be full sooner or later. In summary, our basic ideas of architectural design are that provide dependable and distributed storage image with the help of Hadoop DFS and finish LFS(Log Structured FileSystem) stuffs based on Hadoop DFS.


## HLFS log's create and index ##
> Log is the basic unit for our datas' persistence. In fact, to the need of synchro writing, we make a new log by each writing action, which has different size each time.
> The contents of log include data block, metadata(indirect block,etc) information, and metadata's meta-information(inode). In this way, we can finish indexing one information correctly.

### LOG create ###
  * Any change of files or directories should write into a log with following portions. And they must write in-order semantics -- The aim is to recovery datas consistently if has any crashes.
  1. Any change or new data block (Data-block is smallest operation unit; configurable; size is 8KB or bigger)
  1. Indirect block update (Also named as meta-data block, is inode's 2/3/4 inodex data blocks)
  1. Inode update (Each inode is corresponding a file. If we only afford one file, we need only one inode)
  1. Inode map update (Sinal file has only one Inode map entry)
  * Log's constructure: **A (data）| B (meta-data block log) | C (inode log) | D(inode\_map log)**

### Data Index ###
> Read file's lastest datas: Firstly, find the lastest inode map entry. Secondly, find the corresponding inode. Thirdly, find file's logic address's physical address of the datablock(segment number and segment offset). At last, read datas.
> The latest Inode map entry's location should write into checkpoint file(see fault recovery for details). HLFS read them when it is initial; If it is running, Inode map entry structure should stay in RAM.
  * NOTE: Data block(Can be configured) is changable, like 8KB. If change size less than DB size, it will read a DB and change DB and take append opration at last.

### Space management - Segment ###
  * Split into Segments -- In order to manage and recycle space, we have to split space into segments; Each segment size should not be stationary, and it has no hard regulations for the segment size.
    * Each segment is mapped as a Hadoop DFS file, 64 M (configurable); The name of a segment file should be segno.seg, which the 'segno' increases by 0;
    * Data block storage address (storage address) is 64 bits. Front parts are segment numbers and left parts are offset of a segment.(If segment size is 64MB, that is to say front parts are 26 bits).

  * Segment recycle -- (Segment Clean)It will bring much old datas when we append logs in sequence. So we have to design a recycle mechanism for old ones. In general, the recycle thoughts are all the data blocks' traversal in a segment, which will find the if the current inode index blocks directed to it. If not we we will remove this one. If one segment has a little usage data blocks, we will take the way, 'copy and remove' -- append a new log with the usage datas and remove that segment.

We can divide the recycle to two steps:
  1. Calculate the usage ones in a segment (Get the active data blocks in one segment);
  1. Do the remove or 'copy and remove' action if the counts of active data blocks less than horizon(Configurable).
  * "Calculate segment usage" can be realized by a tool, which storage the special usage datas into file segment\_usage.txt. We can take the way 'Pull mode'(Get the segment to local) from the pc of hlfs client to execute the task calculate segment usage stuffs; Also take another one 'Push mode'(map/reduce way for nearby calculations) to execute segment usage conditions in parallel.
  * "Segment Recycle" stuffs can be done by the only thread for hlfs so it can avoid change inode datas in parallel. Recycle is a low level task so writing jobs will be done at first. When there is no writing tasks we will carry out recycle actions.

### Snapshot realize ###
Log structure filesystem is born with snapshot function. Each log(Not recycled) we write can be a nature snaposhot. That is to say, we can recover that time's datas, which need only load the snapshot's inode. So it can rollback to any time's. But we will not keep all the datas. We suggest users should give a checkpoint for their key datas, which will get a permanent snapshot(contains inode address, take snapshot time, snapshot name) that is recorded into the file named checkpoint.txt. The inode has a snapshot will not be recycled.

## The files HLFS need(All created by appending) ##
  * File SEGMENT -- The main data is storaged here, see 'Space management - Segment' for details.
  * File Superblock -- Record HLFS's configure datas, like data block size, segment size, etc. If it is set, it will be unchangeable. See sources of tool mkfs.hlfs for details.
  * File Segments\_usage.txt -- Record each segment's usage condition. Every line is only for one segment.
  * File Segments\_delmark.txt -- Record removed segments information. Once a segment is removed, it will write down in this file.
  * File snapshot.txt -- Record snapshots information, which are different time's log records.
  * File alive\_snapshot.txt -- Record the current time's active snapshot, it is useful for our tree snapshots usage. See http://code.google.com/p/cloudxy/wiki/HlfsSnapshotDesign for details.

### The key APIS ###
The key APIS have following characters:
  1. C-Style APIS;
  1. Thread-Safe (Because each accessor maintain his own HLFS\_CTRL context)
  1. The File-System of interfaces (HLFS has only one file, so no any concepts about file name)

  * HLFS\_CTRL\*init\_hlfs(const char\*uri)
  * int deinit\_hlfs(HLFS\_CTRL\*ctrl)
  * int hlfs\_stat  (HLFS\_CTRL\*ctrl,HLFS\_STAT\_T\*stat)
  * int hlfs\_open  (HLFS\_CTRL\*ctrl,int flag)
  * int hlfs\_close (HLFS\_CTRL\*ctrl)
  * int hlfs\_read  (HLFS\_CTRL\*ctrl,char\*read\_buf,uint32\_t read\_len,uint64\_t pos)
  * int hlfs\_write (HLFS\_CTRL\*ctrl,char\*write\_buf,uint32\_t write\_len,uint64\_t pos)

## Some shortages about our project ##
  1. It is rugged for memory management -- We have not optimized memory usage so it has something wastes (Even some memory leak somewhere). For this reason, you should give a loose memory size as you can. If you run a dom0 VM, the memory size is better for 1GB.(512MB may be less).
  1. The connections management have not been pooled.It exists the problem that open repeatedly.The connection means its connected to back-end storage (the connection handle of hdfs).This kind of repeatedly open and close action  will waste much time on the network. So we will reduce this work.
  1. For the segment clean operations, it is possible to access the removed segment, which we take the way 'Reference Technique Protect'.Now, we take the simple way 'lock technique', which we add lock when read and write. While there is no I/O requests, we will do clean operations. We will improve our realization ways later on.
  1. If you don't call close API when you close a hlfs file(hdfs way) ,you can't find the file in other connection contexts.This bug will bring a problem(note:you don't although see the file in namespace but you can see the file by executing command 'hadoop fs -tail'.Becouse the data, it read from inputstream, is not been submitted to the name node). Libhdfs has no a convenient API for reading datas from inputstream, so we use a common way, which execute command 'hadoop fs cp' to recovery uncommitted datas.

---

Translated By Harry Wei <harryxiyou@gmail.com>, Dai Jinwei <daijiwei41@gmail.com>，Zhang Jingpeng, Feng Siyu. 2012，3,2