/*
 * HLFS(Log-structured File System based on HDFS) is a file system in 
 * user space which makes use of HDFS files as its virtual disk.The 
 * characteristic of HLFS is log-structured.It means the data write to 
 * HLFS must be appended in the tail of the virtual disk,thereby speeding
 * up both file writing and crash recovery.
 * 
 * Copyright (C) 2012 KangHua <kanghua151@gmail.com>
 * Maintainers:
 * Harry Wei <harryxiyou@gmail.com>
 * Kelvin <kelvin.xupt@gmail.com>
 * 
 * Mail List : cloudxy@googlegroups.com
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 as published by
 *  the Free Software Foundation.
 */

#ifndef _HDFS_LFS_H_
#define _HDFS_LFS_H_
#include "comm_define.h"
#include "base_ops.h"
#include "snapshot.h"


#endif 
