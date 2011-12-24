/*
 * HLFS(Log-structured File System based on HDFS) is a file system in 
 * user space which makes use of HDFS files as its virtual disk.The 
 * feature of HLFS is log-structured.It means the data write to HLFS
 * must be appended in the tail of the virtual disk,thereby speeding
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
#include "hlfs_ctrl.h"
#include "ctrl_region.h"

typedef struct hlfs_ctrl  HLFS_CTRL;

#ifdef __cplusplus  
extern "C" {
#endif
//HLFS_CTRL * init_hlfs(const char *uri, const char *fs_name);

/*
 * init_hlfs: Initialize the ctrl structure according to the uri.
 * @param uri: The path of backend storage. 
 * URI format like:
 * local:///tmp/testenv/testfs
 * hdfs:///tmp/testenv/testfs
 * hdfs://localhost:8020/tmp/testenv/testfs
 * hdfs://localhost/tmp/testenv/testfs
 * hdfs://192.168.0.1:8020/tmp/testenv/testfs
 * @return value: The pointer of the structure HLFS_CTRL will be return.
 */
HLFS_CTRL * init_hlfs(const char *uri);

/*
 * deinit_hlfs: Free the ctrl structure.
 * @param ctrl: The pointer of ctrl structure.
 * return value: Return 0 on success, else return -1.
 */
int deinit_hlfs(HLFS_CTRL *ctrl);

/*
 * hlfs_stat: Fill the structure stat
 * @param ctrl: The global control structure.
 * @param stat: The structure will be filled.
 * @return value: 0 on success, else -1.
 */
int hlfs_stat  (HLFS_CTRL *ctrl,HLFS_STAT_T *stat);

/*
 * hlfs_lstat: Fill the structure stat without initialize HLFS.
 * @param uri: Location of HLFS.
 * @param flag: 0 readonly; 1 writable.
 * @return value: Return 0 on success,else return -1.
 */
int hlfs_lstat (const char*uri,HLFS_STAT_T *stat);

/*
 * hlfs_open: Load the last inode infomation to the structure ctrl.
 * @param ctrl: The global control structure.
 * @param flag: 0 readonly; 1 writable.
 * @return value: Return 0 on success,else return -1.
 */
int hlfs_open  (HLFS_CTRL *ctrl , int flag);

/*
 * hlfs_close: Close the log_write_task thread and all file handles.
 * @param ctrl: The global control structure.
 */
int hlfs_close (HLFS_CTRL *ctrl);

/*
 * hlfs_read: Read data from the virtual disk to memory.
 * @param ctrl: The global control structure.
 * @param read_buf: The location in memory will store the data.
 * @param read_len: The size of data.
 * @param pos: The address of the data in the virtual disk.
 * @return value: Return the number of bytes read on success,else reutrn -1.
 */
int hlfs_read  (HLFS_CTRL* ctrl,char* read_buf,uint32_t read_len,uint64_t pos);

/*
 * hlfs_write: Write data to the virtual disk.
 * @param ctrl: The global control structure.
 * @param write_buf: The location in memory of the data.
 * @param write_len: The size of data.
 * @param pos: The address in the virtual disk will stores the data.
 * @return value: Return the number of bytes written on success,else reutrn -1.
 */
int hlfs_write (HLFS_CTRL* ctrl,char* write_buf,uint32_t write_len,uint64_t pos);

/*
 * hlfs_set_user_ctrl_region: Set the ctrl_region in the structure ctrl 
 * according to the given ctrl_region.
 * @param ctrl: The global control structure.
 * @param ctrl_region: The given ctrl_region structure.
 * @return value: Return 0 on success.
 */
int hlfs_set_user_ctrl_region(HLFS_CTRL * ctrl,CTRL_REGION_T *ctrl_region);

/*
 * hlfs_clean_start: Set 1 to the variable ctrl->ctrl_region->is_start_clean.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_clean_start(HLFS_CTRL *ctrl);

/*
 * hlfs_clean_stop: Set 0 to the variable ctrl->ctrl_region->is_start_clean.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_clean_stop(HLFS_CTRL *ctrl);

/*
 * hlfs_set_clean_level: Set given value to the variable 
 * ctrl->ctrl_region->copy_waterlevel.
 * @param ctrl: The global control structure.
 * @return value: Return 0 on success.
 */
int hlfs_set_clean_level(HLFS_CTRL *ctrl,unsigned int alive_bytes);

/*
 * take_snapshot: Take snapshot with given snapshot name
 * @param ctrl: The global control structure
 * @param ss_name: Snapshot name given by user
 * @return value:
 *  0 --- on success
 * -1 --- creat ss file error
 * -2 --- open ss file error
 * -3 --- append error
 * 
 */
int take_snapshot(HLFS_CTRL *ctrl, const char *ss_name);

/*
 * rm_snapshot: Delete snapshot matching the given name
 * @param uri: The location of hlfs
 * @param ss_name: The snapshot name given by user
 * @return value: 
 *  0 --- on success
 * -1 --- creat ss delmark file error
 * -2 --- open ss delmark file error
 * -3 --- append error
 */
int rm_snapshot(const char *uri, const char *ss_name);

/*
 * find_inode_before_time: find inode created just before given time
 * @param uri: The location of hlfs
 * @param time: The time given by user
 * @param inode_addr: inode address found will be here
 * @return value: 
 *  0 --- on success
 * -1 --- list dir error 
 * -2 --- find nothing
 * -3 --- find inode in seg error
 */
int find_inode_before_time(const char *uri, uint64_t time, uint64_t *inode_addr);

/*
 * find_inode_by_name: find inode matching the snapshot name given by user
 * @param uri: The location of hlfs
 * @param ss_name: The snapshot name given by user
 * @param inode_addr: inode address found will be here
 * @return value: 
 *  0 --- on success
 * -1 --- load all snapshot error 
 * -2 --- find nothing
 */
int find_inode_by_name(const char *uri, const char *ss_name, uint64_t *inode_addr);

/*
 * get_inode_info: get the creat time and file size of given inode
 * @param uri: The location of HLFS
 * @param inode_addr: inode address 
 * @param creat_time: creat time will be here
 * @param length: The length of file will be here
 * @return value:
 * 0 --- on success
 * 1 --- load inode error
 */
int get_inode_info(const char *uri, uint64_t inode_addr, uint64_t *creat_time, \
		uint64_t *length);

/*
 * hlfs_open_by_inode: Roll backward to given inode
 * @param ctrl: The global control structure
 * @param inode_addr: The inode address given by user
 * @param flag: 0 readonly 1 writeable
 * @return value: 
 *  0 --- on success
 * -1 --- parameter error
 * -2 --- HLFS was opened by others
 * -3 --- load inode error
 */
int hlfs_open_by_inode(HLFS_CTRL *ctrl, uint64_t inode_addr, int flag);

/*
 * list all snapshot names to ss_name_array
 * @param uri: The location of HLFS
 * @param ss_name_array: names willl be here
 *  0 --- on success
 * -1 --- snapshot.txt is not exist
 * -2 --- open ss file error
 * -3 --- read ss file error
 * -4 --- read ss delmark file error
 */
int list_all_snapshot(const char *uri, char **ss_name_array);
#ifdef __cplusplus 
} 
#endif 
#endif 
