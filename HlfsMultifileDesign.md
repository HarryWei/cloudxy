# 简介 #
### 编写目的 ###
对HLFS多文件扩展的总体框架进行设计，明确多文件扩展的目标，设计原则，以及需要完成的核心功能和设计核心要点等等，在此基础上可设计详细设计文档。
### 背景介绍 ###
HLFS(Hadoop DFS Log-structured FileSystem)继承了HDFS和LFS的优点，支持随机读写，快照回滚，块压缩，高速缓存等功能。目前，HLFS只支持单文件操作（类似Amazon的EBS），为弹性云平台提供虚拟磁盘，可以称为块级别的存储系统，但是单文件毕竟是存在限制的，例如，单文件的大小是固定的（除非扩大索引结构），不方便多个用户同时使用等等。所以本课题所解决的问题就是把HLFS扩展为文件系统级别的存储系统，使HLFS应用场合更加广泛，使用更加灵活，成为真正的文件系统。

### 实现简述 ###
先实现使HLFS支持两个文件的随机读写功能（这个功能无需添加目录结构，但是需要实现文件操作主要接口，详见2.2，本概要设计主要是针对添加目录结构后文件操作以及目录操作的设计），然后再在其上添加目录结构使HLFS支持目录结构和多文件随机读写功能。

# 总体设计 #
### 特性描述 ###
  * 无限磁盘容量 ----- 块级别的HLFS只支持一个文件，当这个文件达到了最大容量，将不能继续存储数据，扩展后可实现“无限磁盘容量”，当一个文件超出最大容量之后，可创建新的文件存储数据。
  * 支持多文件读写和文件操作 ----- 可实现多文件随机读写，快照等操作，用户可直接对文件进行操作（文件管理，实现hlfs\_create(), hlfs\_fopen, hlfs\_fclose, hlfs\_fwrite, hlfs\_fread等文件操作）。
  * 支持目录结构 ----- 添加了目录结构之后，HLFS就具备管理目录的功能（管理目录结构，就是实现mkdir, rmdir, rename, link, unlink, symlink等），用户就可进行目录操作。
  * 支持文件重名 ----- 添加了目录结构之后，可在不同目录下创建重名文件。
  * 支持树形目录结构 ----- 添加了目录结构之后，可以实现树形目录管理结构。
  * 文件系统 ----- 单文件HLFS还不能称为真正的文件系统，扩展后的HLFS，可以真正的称为文件系统而且还具备很多新的特性。

### 多文件随机读写和文件操作 ###
用户使用HLFS提供的新接口（文件操作接口，见"HLFS 目录接口和文件接口设计" ）可以实现文件的随机读写和文件操作，读写时先通过路径名找到指定的文件（具体过程与unix/unix-like的方式一样，从已知的’/’依次向下找），然后进行读写和操作，详见概要设计。

### 目录操作 ###
要实现对目录的操作，HLFS必须支持目录结构，需要维护一个dentry结构， 同时inode field需要添加一个字段用于说明这个log是对文件还是目录的修改，可能还需要作出其他修改，详见概要设计。

### 概要设计 ###
#### 多文件扩展核心思路 ####
HLFS多文件扩展需要维护文件系统整个目录结构，如果把整个目录结构放到log，那么每次追加log将变得异常繁琐，所以目前做法是，把目录结构与log结构拆分管理，目录结构通过一个外部文件dentry维护，dentry文件中每行放入一个
```
Struct dentry {
　　uint64_t inode_no;
　　char file_name[HLFS_FILE_NAME_MAX];
　　uint64_t is_alive;
};
```
同时在inode结构中添加is\_dir字段，用于说明本文件是否为目录。还要在ctrl结构体中维护’/’的inode\_addr，方便查找等操作。
```
说明：
1，dentry 的inode_no字段用于说明文件所对应的inode号。
2, dentry的file_name字段用于说明inode所对应的文件件名。
3，is_alive用于说明此dentry记录是否有效，因为dentry文件要及时clean。
```

#### 目录管理和文件管理 ####
```
场景一：
如果第一次启动HLFS首先创建dentry文件，然后追加log结构，最后向dentry文件中写入’/’目录的dentry结构。然后把’/’的inode addr找到，初始化ctrl中’/’的inode_addr。以后每次更新’/’下文件或者目录都要更新’/’对应的log，并且要更新ctrl结构体中’/’的inode_addr。（也可以在ctrl中维护’/’的dentry结构，如果这样，每次查找文件都要通过’/’的inode_addr，并且’/’的inode_no固定设置为1，这样就没要在ctrl中维护dentry结构了。）

场景二：
如果不是第一次启动HLFS，那么首先通过dentry文件，找到最新的’/’的dentry记录，然后通过’/’的inode_no找到对应的最新的log，进而找到’/’的inode_addr，初始化ctrl。

注意：
创建或者删除一个文件，需要追加两个log，对应dentry文件中两个新加记录，具体如下：

1, 创建/file1, 那么需要追加file1的log，同时更新’/’的log，这是因为’/’的db(ib)要更新。
2，删除/file1，需要找到file1的log，然后回收，更新’/’的log，还要在dentry文件中增加一条dentry结构记录，把/file1的dentry置为无效。
3，其他情况同理。
```

### HLFS 目录接口和文件接口设计 ###
```
int hlfs_create(struct hlfs_ctrl *ctrl, char *f_path)；
创建一个hlfs文件

int hlfs_remove(struct hlfs_ctrl *ctrl, char *f_path)；
删除一个hlfs文件

int hlfs_list(struct hlfs_ctrl *ctrl)；
列出所有hlfs文件和目录

int hlfs_fopen(struct hlfs_ctrl *ctrl, char *f_path, int flag)；
打开一个hlfs文件，就是先找到这个文件（通过f_path）,然后初始化ctrl结构（初始化inode,im,rw_inode_flag 等字段）。

int hlfs_fwrite(struct hlfs_ctrl *ctrl, char *f_path, char *write_buf, uint32_t write_len, uint64_t pos);
根据f_path找到文件的inode_no，然后和ctrl中的对比，如果一致，说明此文件已打开，则可写数据，写的过程和单文件一样。如果不一致，需要先打开此文件。

int hlfs_fread(struct hlfs_ctrl *ctrl, char *f_path, char *read_buf, uint32_t read_len, uint64_t pos);
根据f_path找到文件的inode_no，然后和ctrl中的对比，如果一致，说明此文件已打开，则可读数据，读的过程和单文件一样。如果不一致，需要先打开此文件。

int hlfs_fclose(struct hlfs_ctrl *ctrl, char *f_path);
释放打开文件所占的资源。

　　以上接口是HLFS文件操作接口，从图1可以清晰看出HLFS目录结构图，文件接口中可以根据f_path字段搜索到要进行操作文件的inode no（从’/’开始，方式和unix方式一样）, 然后找到对应inode,最后进行具体操作。(目前只支持绝对路径)

int hlfs_mkdir(struct hlfs_ctrl *ctrl, char *dir_path)；
追加新的log（dir_path对应），更新dir_path所在的上一级目录所对应的log，追加dir_path所对应的dentry结构到dentry文件中，当dir_path所对应的上一级目录是’/’时，还需要更新ctrl结构。

int hlfs_rmdir(struct hlfs_ctrl *ctrl, char *dir_path)；
此操作只能删除空目录。 更新dir_path所在的上一级目录所对应的log，追加dir_path所对应的dentry结构到dentry文件中，状态置为无效，当dir_path所对应的上一级目录是’/’时，还需要更新ctrl结构。

int hlfs_rename(struct hlfs_ctrl *ctrl, char *old_dir_path, char *new_dir_path)；
只需在dentry文件中找到old_dir_path，然后修改为new_dir_path。

注意：上面大都描述的是正常情况，异常情况需要给出不同的处理情况，具体实现时再处理。目前先提供这些主要接口，如有需求还会提供其他接口。
```

# HLFS多文件扩展概要设计总结 #
以上的设计可能还存在一些没有考虑到的问题，在实践过程或者后期讨论过程中可能还会不断的更新，尤其是添加目录结构后目录和文件的操作，这里在实践过程中可能需要变动。


---

Written by Harry Wei <harryxiyou@gmail.com>