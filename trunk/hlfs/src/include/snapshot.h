/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *  Maintainers:
  * 		Harry Wei <harryxiyou@gmail.com>
  * 		Kelvin <kelvin.xupt@gmail.com>
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __HLFS_SNAPSHOT_H_
#define __HLFS_SNAPSHOT_H_

#include <stdint.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"

#define	SNAPSHOT_FILE "snapshot.txt"
#define	ALIVE_SNAPSHOT_FILE "alive_snapshot.txt"

struct snapshot {
	uint64_t timestamp;
	uint64_t inode_addr;
	char sname[HLFS_FILE_NAME_MAX];
	char up_sname[HLFS_FILE_NAME_MAX]; /* for tree style snapshot */
} __attribute__((packed));

#define SS_ITEM_SEP 			"@@##$$"


#ifdef __cplusplus  
extern "C" {
#endif

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
//struct snapshot *__hlfs_get_all_snapshots(struct back_storage *storage,int *num_entries);


/**
 * hlfs_take_snapshot: take a snapshot given a snapshot name
 * @para ctrl: the hlfs global control construction
 * @para ssname: the snapshot's name
 * @return value: 0 is right, -1 is wrong
 */
int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname);
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

#ifdef __cplusplus 
} 
#endif 


#endif 
