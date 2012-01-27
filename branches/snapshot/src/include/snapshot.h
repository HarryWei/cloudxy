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

#define SS_ITEM_SEP @@##$$



#endif 
