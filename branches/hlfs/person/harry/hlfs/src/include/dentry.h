/*
  *  Copyright (C) 2012 Harry Wei <harryxiyou@gmail.com>
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __HLFS_DENTRY_H_
#define __HLFS_DENTRY_H_

#include <stdint.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"

#define	DENTRY_FILE 					"dentry.txt"
#define DENTRY_ALIVE					((uint64_t) 0)
#define DENTRY_DEAD						((uint64_t) 1)
#define HLFS_DIR						((uint64_t) 0)
#define HLFS_FILE						((uint64_t) 1)
#define HLFS_ROOT_INODE_NO				((uint64_t) 1)

struct dentry {
	uint64_t inode_no;
	char file_name[HLFS_FILE_NAME_MAX];
//	uint64_t is_alive;
} __attribute__((packed));

#define HD_ITEM_SEP 			"@@##$$"

#ifdef __cplusplus  
extern "C" {
#endif

int hlfs_list(struct hlfs_ctrl *ctrl);
int hlfs_create(struct hlfs_ctrl *ctrl, const char *f_path, int is_dir);
int hlfs_remove(struct hlfs_ctrl *ctrl, const char *f_path);
int hlfs_fopen(struct hlfs_ctrl *ctrl, const char *f_path, int flag);
int hlfs_fclose(struct hlfs_ctrl *ctrl, const char *f_path);
int hlfs_fwrite(struct hlfs_ctrl *, const char *, char *, uint32_t, uint64_t);
int hlfs_fread(struct hlfs_ctrl *, const char *, char *, uint32_t, uint64_t);
/**
 * hlfs_find_inode_by_name: find a inode in light of sname
 * @para uri: the hlfs storage path
 * @para sname: the sname of inode for searching
 * @para inode_addr: get the inode's inode addr
 * @return value: 0 is right, -1 is wrong, 1 is no this snapshot name
 */
#if 0
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
#endif
#ifdef __cplusplus 
} 
#endif 


#endif 
