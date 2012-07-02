 /*
   *  Copyright (C) 2012      Kelvin <kelvin.xupt@gmail.com>
   *
   *  This program is free software; you can redistribute it and/or modify it
   *  under the terms of the GNU General Public License version 2 as published by
   *  the Free Software Foundation.
  */
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include <stdint.h>
#include "storage.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "snapshot.h"

#define MAX_BUFSIZE (4 * 1024)

struct snapshot *__hlfs_get_all_snapshots(struct back_storage *storage,int *num_entries);
/*
 */
struct snapshot *hlfs_get_all_snapshots(const char *uri,int *num_entries)
{
	//HLOG_DEBUG("enter func %s", __func__);
	if (NULL == uri || NULL == num_entries) {
		HLOG_ERROR("Parameter Error!");
		return NULL;
	}


	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		return NULL;
	}
	struct snapshot * snapshot =  __hlfs_get_all_snapshots(storage,num_entries);
	deinit_storage_handler(storage);
	return snapshot;
}

struct snapshot *__hlfs_get_all_snapshots(struct back_storage *storage,int *num_entries){
	//HLOG_DEBUG("enter func %s", __func__);
	if (NULL == storage || NULL == num_entries) {
		HLOG_ERROR("Parameter Error!");
		return NULL;
	}
	char *snapshots_buf = NULL;
	int ret = 0;
	GHashTable *ss_hashtable = g_hash_table_new(g_str_hash, g_str_equal);
    GList *snapshot_list = NULL;
	
	ret = load_all_snapshot(storage, SNAPSHOT_FILE, ss_hashtable);
	if (ret < 0) {
		HLOG_ERROR("load all ss error: %d", ret);
		goto out;
	}
    HLOG_DEBUG("hash table:%d",g_hash_table_size(ss_hashtable));  
    sort_all_snapshot(ss_hashtable,&snapshot_list);
	if (0 == g_list_length(snapshot_list)) {
		HLOG_ERROR("the length of snapshot list is 0");
		goto out;
	}
	snapshots_buf = (char *)g_malloc0(g_list_length(snapshot_list)*sizeof(struct snapshot));
	if (NULL == snapshots_buf){
		g_message("Allocate error!");
		goto out;
	}
    int i=0;
    int offset=0;
    for(i = 0; i < g_list_length(snapshot_list); i++){
        struct snapshot *ss = g_list_nth_data(snapshot_list,i);
		HLOG_DEBUG("999 ss->timestamp is %llu, ss->inode_addr is %llu, ss->sname is %s, ss->up_sname is %s",
							ss->timestamp, ss->inode_addr, ss->sname, ss->up_sname);
        memcpy(snapshots_buf+offset,ss,sizeof(struct snapshot));
        offset +=sizeof(struct snapshot);
    }
    *num_entries = g_list_length(snapshot_list);
out: 
	g_free(storage);
//	g_hash_table_destroy(ss_hashtable);
    if(snapshot_list!=NULL){
        g_list_free(snapshot_list);
    }
	//HLOG_DEBUG("leave func %s", __func__);
	return (struct snapshot *) snapshots_buf;
}

