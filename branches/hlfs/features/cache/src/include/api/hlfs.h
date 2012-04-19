/*
 * HLFS(Log-structured File System based on HDFS) is a file system in 
 * user space which makes use of HDFS files as its virtual disk.The 
 * characteristic of HLFS is log-structured.It means the data write to 
 * HLFS must be appended in the tail of the virtual disk,thereby speeding
 * up both file writing and crash recovery.
 * 
 * Designed by Kang Hua <kanghua151@msn.com>
 * Maintainers:
 * Harry Wei <harryxiyou@gmail.com>
 * Kelvin <kelvin.xupt@gmail.com>
 * Zhangbo <0311zhangbo@gmail.com>
 * 
 * Mail List : cloudxy@googlegroups.com
 */
#ifndef _HDFS_LFS_H_
#define _HDFS_LFS_H_
#include "cache.h"
#include "hlfs_ctrl.h"
#include "ctrl_region.h"
#include "comm_define.h"

typedef struct hlfs_ctrl  HLFS_CTRL;
typedef struct cache_ctrl  CACHE_CTRL;

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
HLFS_CTRL *init_hlfs(const char *uri);

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

/**
 * hlfs_find_inode_before_time: find a inode nearby the timestamp
 * @para uri: the hlfs storage path
 * @para timestamp: the timestamp of inode for searching
 * @para inode_addr: get the inode's inode addr
 * @return value: 0 is right, -1 is wrong, 1 is no segfiles in back storage
 */
int hlfs_find_inode_before_time(const char *uri, uint64_t timestamp, uint64_t *inode_addr);

/**
 * hlfs_get_inode_info: get a inode info in light of its addr
 * @para uri: the hlfs storage path
 * @para inode_addr: the addr of inode for searching
 * @para ctime: get the inode's create time
 * @para length: get the inode's length
 * @return value: 0 is right, -1 is wrong
 */
int hlfs_get_inode_info(const char *uri, uint64_t inode_addr, uint64_t *ctime, uint64_t *length);

/**
 * hlfs_open_by_inode: load a inode in light of flag
 * @para ctrl: the hlfs global control construction
 * @para inode_addr: get the inode's inode addr
 * @para flag: 0 is common style, 1 is readonly
 * @return value: 0 is right, -1 is wrong
 */
int hlfs_open_by_inode(struct hlfs_ctrl *ctrl, uint64_t inode_addr, int flag);
int hlfs_open_by_snapshot(struct hlfs_ctrl *ctrl,const char* snapshot,int flag);

/**
 * hlfs_find_inode_by_name: find a inode in light of sname
 * @para uri: the hlfs storage path
 * @para sname: the sname of inode for searching
 * @para inode_addr: get the inode's inode addr
 * @return value: 0 is right, -1 is wrong, 1 is no this snapshot name
 */
int hlfs_find_inode_by_name(const char *uri, const char *sname, uint64_t *inode_addr);

/**
 * hlfs_rm_snapshot: delete a snapshot in light of sname
 * @para uri: the hlfs storage path
 * @para ssname: the sname of inode for deleting
 * @return value: 0 is right, -1 is wrong, 1 is ssname not exist
 */
int hlfs_rm_snapshot(const char *uri, const char *ssanme);

struct snapshot* hlfs_get_all_snapshots(const char *uri,int *num_entries);

/**
 * hlfs_take_snapshot: take a snapshot given a snapshot name
 * @para ctrl: the hlfs global control construction
 * @para ssname: the snapshot's name
 * @return value: 0 is right, -1 is wrong
 */
int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname);

#ifdef __cplusplus 
} 
#endif 
#endif 
