#ifndef _HLFS_SNAPSHOT_H_
#define _HLFS_SNAPSHOT_H_

#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include "hlfs_log.h"
#include "storage.h"
#include "hlfs_ctrl.h"

#define SS_FILE "snapshot.txt"
#define MAX_SS_NAME_LEN 128

struct snapshot {
	char ss_name[MAX_SS_NAME_LEN];
	char up_ss_name[MAX_SS_NAME_LEN];
	uint64_t version;
	struct inode_map_entry ime;
}__attribute__((packed));

#ifdef __cplusplus
extern "C" {
#endif

/*Append the will-be deleted snapshot's ss_name to the ss_delmark.txt*/
int append_ss_delmark(struct back_storage *storage, const char *ss_name);

/*Change the format of the structure snapshot to a character string*/
int ss2text(struct snapshot *ss, char *buf, int flag);

/*Append the character string matching a snapshot structure to the snapshot.txt*/
/*the buf size should be (sizeof(struct snapshot) + 6 )*/
int dump_ss_text(struct back_storage *storage, const char *buf);

/*Append the snapshot structure to the file snapshot.txt*/
int dump_ss(struct back_storage *storage, struct snapshot *ss, int flag);

/*Change the format of a character string to the structure snapshot*/
int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag);

/*Load all structure snapshot to a Hash table from the snapshot.txt*/
int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable);

/*Load the structure snapshot matching a given name from the snapshot.txt*/
int load_ss_by_name(struct back_storage *storage, struct snapshot *ss, \
		const char *ss_name);

#ifdef __cplusplus
}
#endif

#endif
