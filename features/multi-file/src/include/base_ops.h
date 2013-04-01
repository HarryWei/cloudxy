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
 * Zhangbo <0311zhangbo@gmail.com>
 * 
 * Mail List : cloudxy@googlegroups.com
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 as published by
 *  the Free Software Foundation.
 */



#ifndef _HDFS_BASE_H_
#define _HDFS_BASE_H_

#include "cache.h"
#include "hlfs_ctrl.h"
#include "ctrl_region.h"
#include "comm_define.h"

typedef struct hlfs_ctrl  HLFS_CTRL;
#ifdef __cplusplus  
extern "C" {
#endif
//HLFS_CTRL * init_hlfs(const char *uri, const char *fs_name);
/**
 * init_hlfs: Initialize the ctrl structure according to the uri.
 * @param uri: The path of backend storage. 
 * URI format like:
 * local:///tmp/testenv/testfs
 * hdfs:///tmp/testenv/testfs
 * hdfs://localhost:8020/tmp/testenv/testfs
 * hdfs://localhost/tmp/testenv/testfs * hdfs://192.168.0.1:8020/tmp/testenv/testfs * @return value: The pointer of the structure HLFS_CTRL will be return.
 */
HLFS_CTRL * init_hlfs(const char *uri);
HLFS_CTRL * init_hlfs_by_config(const char *config_file_path);
 
/**
 * deinit_hlfs: Free the ctrl structure.
 * @param ctrl: The pointer of ctrl structure.
 * return value: Return 0 on success, else return -1.
 */
int deinit_hlfs(HLFS_CTRL *ctrl);

/**
 * hlfs_stat:
 */
int hlfs_stat  (HLFS_CTRL *ctrl,HLFS_STAT_T *stat);

/**
 * hlfs_lstat:
 */
int hlfs_lstat (const char*uri,HLFS_STAT_T *stat);

/**
 * hlfs_open: Load the last inode infomation to the structure ctrl.
 * @param ctrl: The global control structure.
 * @param flag: 0 readonly; 1 writable.
 * @return value: Return 0 on success,else return -1.
 */
int hlfs_open  (HLFS_CTRL *ctrl , int flag);

/**
 * hlfs_close: Close the log_write_task thread and all file handles.
 * @param ctrl: The global control structure.
 */
int hlfs_close (HLFS_CTRL *ctrl);

/**
 * hlfs_read: Read data from the virtual disk to memory.
 * @param ctrl: The global control structure.
 * @param read_buf: The location in memory will store the data.
 * @param read_len: The size of data.
 * @param pos: The address of the data in the virtual disk.
 * @return value: Return the number of bytes read on success,else reutrn -1.
 */
int hlfs_read  (HLFS_CTRL* ctrl,char* read_buf,uint32_t read_len,uint64_t pos);

/**
 * hlfs_write: Write data to the virtual disk.
 * @param ctrl: The global control structure.
 * @param write_buf: The location in memory of the data.
 * @param write_len: The size of data.
 * @param pos: The address in the virtual disk will stores the data.
 * @return value: Return the number of bytes written on success,else reutrn -1.
 */
int hlfs_write (HLFS_CTRL* ctrl,char* write_buf,uint32_t write_len,uint64_t pos);

int hlfs_flush(struct hlfs_ctrl *ctrl);
/**
 * hlfs_set_user_ctrl_region: Set the ctrl_region in the structure ctrl 
 * according to the given ctrl_region.
 * @param ctrl: The global control structure.
 * @param ctrl_region: The given ctrl_region structure.
 * @return value: Return 0 on success.
 */
int hlfs_set_user_ctrl_region(HLFS_CTRL * ctrl,CTRL_REGION_T *ctrl_region);

/**
 * hlfs_clean_start: Set 1 to the variable ctrl->ctrl_region->is_start_clean.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_clean_start(HLFS_CTRL *ctrl);

/**
 * hlfs_clean_stop: Set 0 to the variable ctrl->ctrl_region->is_start_clean.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_clean_stop(HLFS_CTRL *ctrl);

/**
 * hlfs_set_clean_level: Set given value to the variable 
 * ctrl->ctrl_region->copy_waterlevel.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_set_clean_level(HLFS_CTRL *ctrl,unsigned int alive_bytes);

#ifdef __cplusplus 
} 

#endif 
#endif 

