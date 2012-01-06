#ifndef __HLFS_SNAPSHOT_HELPER_H_
#define __HLFS_SNAPSHOT_HELPER_H_

#include <stdint.h>
#include <stdio.h>
#include "snapshot.h"
#include "hlfs_ctrl.h"

// for basic snapshot
int snapshot2text(const struct snapshot* snapshot, char*textbuf);
int dump_snapshot(struct back_storage *storage,const char* snapshot_file,struct snapshot * snapshot);
int snapshot_delmark2text(const char* ssname, char*textbuf);
int dump_snapshot_delmark(struct back_storage *storage,const char* snapshot_file,const char* ssnamea);
int load_snapshot_from_text(struct back_storage *storage,const char* snapshot_file,const char* snapshot_textbuf);
// for tree snapshot
// find inode_addr's snapshot name
int find_up_ss_name_of_inode(struct hlfs_ctrl *, uint64_t , char **);
#endif 
