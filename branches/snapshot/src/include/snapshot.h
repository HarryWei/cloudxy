#ifndef __HLFS_SNAPSHOT_H_
#define __HLFS_SNAPSHOT_H_

#include <stdint.h>
#include <hlfs_ctrl.h>
#include <stdio.h>
#include "comm_define.h"

#define SNAPSHOT_FILE 	            	"snapshot_usage.txt"
#define SNAPSHOT_DELMARK_FILE 			"snapshot_delmark.txt"

struct snapshot {
	uint64_t timestamp;
	uint64_t inode_addr;
	char sname[HLFS_FILE_NAME_MAX];
    char up_sname[HLFS_FILE_NAME_MAX]; /*  for tree style snapshot  */
} __attribute__((packed));

int snapshot2text(const struct snapshot* snapshot,char*textbuf);
int dump_snapshot(struct back_storage *storage,const char* snapshot_file,struct snapshot * snapshot);
int load_snapshot_from_text(struct back_storage *storage,const char* snapshot_file,const char* snapshot_textbuf);


#if 0
struct seg_info {
	uint32_t segno;
	uint64_t lmtime;
} __attribute__((packed));
#endif 

#if 0
int hlfs_take_snapshot(struct hlfs_ctrl *ctrl,const char *ssname);
int hlfs_rm_snapshot(const char *uri,const char *ssname);
int hlfs_list_all_snapshots(const char *uri, char **ssname);
int hlfs_find_inode_by_name(const char *uri, const char *sname,uint64_t *inode_addr);
int hlfs_find_inode_before_time(const char *uri, uint64_t timestamp, uint64_t *inode_addr);
int hlfs_open_by_inode(struct hlfs_ctrl *ctrl,uint64_t inode_addr,int flag);
int hlfs_get_inode_info(const char *uri,uint64_t inode_addr,uint64_t *ctime,uint64_t *length);
#endif 
	
#endif
