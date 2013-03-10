/*
  *  Copyright (C) 2013 Harry Wei <harryxiyou@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __HLFS_DENTRY_HELPER_H_
#define __HLFS_DENTRY_HELPER_H_

#include <stdint.h>
#include <stdio.h>
#include "dentry.h"
#include "hlfs_ctrl.h"

#ifdef __cplusplus  
extern "C" {
#endif

// for basic snapshot
int dump_dentry(struct back_storage *storage,const char* dentry_file,struct dentry *dentry);
int load_all_dentry(struct back_storage *storage,const char* dentry_file,GHashTable *ds_hashtable);
int load_dentry_by_name(struct back_storage *storage, const char* dentry_file,struct dentry **ds, const char *ds_name);
#if 0
int dump_snapshot_delmark(struct *storage,const char* snapshot_file,const char* ssname);
int sort_all_snapshot(GHashTable *ss_hashtable,GList **snapshot_list);
int redump_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable);
/* get lateset alive snapshot before timestamp, it will be used when  hlfs open by inode*/
int find_latest_alive_snapshot_before_time(struct back_storage *storage, const char *snapshot_file, const char* alive_snapshot_file, struct snapshot **ss,uint64_t timestamp);
/* get lateset alive snapshot , it will be used when hlfs open */
int find_latest_alive_snapshot(struct back_storage *storage,const char* alive_snapshot_file, const char* snapshot_file,struct snapshot **ss);
/* dump current alive snapshot to alive_snapshot_file, it will be used when do hlfs_open&hlfs_open_by_inode and take snasphot ops*/
int dump_alive_snapshot(struct back_storage* storage,const char *alive_snapshot_file,struct snapshot *snapshot); 

#endif

#ifdef __cplusplus 
} 
#endif 

#endif 
