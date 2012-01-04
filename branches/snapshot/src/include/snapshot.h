#ifndef __HLFS_SNAPSHOT_H_
#define __HLFS_SNAPSHOT_H_

#include <stdint.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"

#define SNAPSHOT_FILE 	            	"snapshot.txt"

struct snapshot {
	uint64_t timestamp;
	uint64_t inode_addr;
	char sname[HLFS_FILE_NAME_MAX]; char up_sname[HLFS_FILE_NAME_MAX]; /*  for tree style snapshot  */
} __attribute__((packed));

#ifdef __cplusplus
extern "C" {
#endif

int snapshot2text(const struct snapshot *snapshot, char *textbuf);
int dump_snapshot(struct back_storage *storage, const char* snapshot_file, \
		struct snapshot *snapshot);
int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag);
int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable);
int load_ss_by_name(struct back_storage *storage, struct snapshot *ss,\
		const char *ss_name);
int snapshot_delmark2text(const char *ss_name, char *textbuf);
int dump_snapshot_delmark(struct back_storage *storage, const char *snapshot_file, \
		const char *ssname);
int find_up_ss_name_of_inode(struct hlfs_ctrl *ctrl, uint64_t inode_addr, char **up_ss_name);
int load_all_ss_use_inode_addr_keys(struct back_storage *storage, \
		GHashTable *ss_hashtable_use_inode_addr_keys);
int load_all_ss_use_up_sname_keys(struct back_storage *storage, \
		GHashTable *ss_hashtable_use_up_sname_keys);

#ifdef __cplusplus
}
#endif

#endif 
