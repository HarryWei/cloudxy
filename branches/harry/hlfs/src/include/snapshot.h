#ifndef __SNAPSHOT_H
#define __SNAPSHOT_H

#include <stdint.h>
#include <hlfs_ctrl.h>
#include <stdio.h>

#define SNAPSHOT_FILE 		"snapshot.txt"
#define SNAME_LEN				(79)

struct snapshot {
	uint64_t timestamp;
	uint64_t inode_addr;
	char sname[SNAME_LEN];
} __attribute__((packed));

#endif
