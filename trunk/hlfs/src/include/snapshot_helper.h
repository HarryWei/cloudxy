/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __HLFS_SNAPSHOT_HELPER_H_
#define __HLFS_SNAPSHOT_HELPER_H_

#include <stdint.h>
#include <stdio.h>
#include "snapshot.h"
#include "hlfs_ctrl.h"

#ifdef __cplusplus  
extern "C" {
#endif

// for basic snapshot
int dump_snapshot(struct back_storage *storage,const char* snapshot_file,struct snapshot * snapshot);
int dump_snapshot_delmark(struct back_storage *storage,const char* snapshot_file,const char* ssname);
int load_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable);
int sort_all_snapshot(GHashTable *ss_hashtable,GList **snapshot_list);
int redump_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable);
int load_snapshot_by_name(struct back_storage *storage, const char* snapshot_file,struct snapshot **ss, const char *ss_name);
/* get lateset alive snapshot before timestamp, it will be used when  hlfs open by inode*/
int find_latest_alive_snapshot_before_time(struct back_storage *storage, const char *snapshot_file, const char* alive_snapshot_file, struct snapshot **ss,uint64_t timestamp);
/* get lateset alive snapshot , it will be used when hlfs open */
int find_latest_alive_snapshot(struct back_storage *storage,const char* alive_snapshot_file, const char* snapshot_file,struct snapshot **ss);
/* dump current alive snapshot to alive_snapshot_file, it will be used when do hlfs_open&hlfs_open_by_inode and take snasphot ops*/
int dump_alive_snapshot(struct back_storage* storage,const char *alive_snapshot_file,struct snapshot *snapshot); 

#ifdef __cplusplus 
} 
#endif 

#endif 
