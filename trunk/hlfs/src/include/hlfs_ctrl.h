/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_H_
#define _HLFS_H_

#include <time.h>
#include <stdint.h>
#include "storage.h"
#include "cmd_define.h"
#include "ctrl_region.h"
#include "cache.h"
#include "icache.h"
#include "clone.h"

#define MAX_FILE_NAME_LEN 128U
struct inode {
	int64_t length;
    	//int32_t uid;
    	//int32_t gid;
    	//int32_t mode;
    	//uint64_t ctime;               /* time of last status change */
    	uint64_t mtime;               /* time of last modification */
    	//uint64_t atime;               /* time of last access */
    	int64_t blocks[12];         /* the first 8KB*12=96KB */
    	int64_t iblock;             /* the next 8KB/8*8KB=8MB */
    	int64_t doubly_iblock;      /* the next 8K/8*8K/8*8K=8GB */
    	int64_t triply_iblock;      /* the next 8K/8*8K/8*8K/8*8K=8TB */
}__attribute__((packed));  

struct inode_map_entry {
	uint64_t inode_no;	              /* inode number */
       uint64_t inode_addr;		/* inode's DISK address */ 
}__attribute__((packed)); 

struct super_block {
    uint32_t seg_size;					/* max segment size */
    uint32_t block_size;		        /* max data block size */
    uint64_t max_fs_size;               /* max filesystem size */
    char fsname[MAX_FILE_NAME_LEN];		/* record the file's name */
};

//#pragma pack (4)
struct log_header {
    //int32_t  version;
    //int64_t  header_checksum;
    //int64_t  data_checksum;
    uint32_t log_size;
    uint64_t ctime;           /* create time */
    uint32_t start_db_no;  /* log first db no*/
    uint32_t db_num;	  /* db amounts */
    uint32_t ib_num;	  /* ib amount */
    char data[0];
}__attribute__((packed)); 

#define LOG_HEADER_LENGTH sizeof(struct log_header)
/*
struct write_req{
    char * req_buf;
    uint32_t db_start;
    uint32_t db_end;
};
struct write_rsp{
    int res;
    uint64_t size;
};
*/

/* The control structure in RAM */
struct hlfs_ctrl {
    struct   super_block sb;	        	/* the sb in RAM */
    struct   inode_map_entry imap_entry;	/* the inode map in RAM */
    struct   inode inode;		
    uint32_t last_segno;				    /* the current segment */
    uint32_t last_offset;				    /* the current offset in current segment */
    struct   back_storage *storage;
    void *   last_wsegfile_handler;
    void *   last_rsegfile_handler;
    uint32_t last_read_segno;
    uint32_t last_rsegfile_offset;          /* last segfile end offset when last open */
    //GAsyncQueue * write_req_aqueue;
    //GAsyncQueue * write_rsp_aqueue;
    //struct write_rsp  write_rsp;
    //struct write_req  write_req;
    uint64_t last_write_timestamp;
    uint64_t last_read_timestamp;
    //int seg_clean_run;
    CTRL_REGION_T * ctrl_region;
    GMutex * hlfs_access_mutex;
    GThread *seg_clean_thread;
    int usage_ref;
    int rw_inode_flag;
    char alive_ss_name[MAX_FILE_NAME_LEN];
    struct cache_ctrl *cctrl;
    struct icache_ctrl *icache;
    uint32_t io_nonactive_period;
    uint32_t start_segno;
    FAMILY_CTRL *family;
};

typedef struct hlfs_stat{
       uint32_t seg_size;					/* max segment size */
       uint32_t block_size;		        /* max data block size */
       uint64_t max_fs_size;               /* max filesystem size */
       char fsname[MAX_FILE_NAME_LEN];		/* record the file's name */
       uint32_t last_segno;
       uint32_t last_offset;
}HLFS_STAT_T;

#endif
