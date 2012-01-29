/*
 * This file implements the snapshot module API hlfs_list_all_snapshots()
 * 
 * By Kelvin <kelvin.xupt@gmail.com>
 *
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

/*
 */
struct snapshot* hlfs_get_all_snapshots(const char *uri,int *num_entries)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal,g_free,g_free);
    GList *snapshot_list = NULL;

	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		g_hash_table_destroy(ss_hashtable);
		return NULL;
	}
	ret = load_all_snapshot(storage, ss_hashtable);
	if (ret < 0) {
		HLOG_ERROR("load all ss error: %d", ret);
		goto out;
	}
   
    sort_all_snapshot(ss_hashtable,snapshot_list);
	char* snapshots_buf = (char *)g_malloc0(g_list_length(snapshot_list)*sizeof(struct snapshot));
	if (NULL == snapshots_buf){
		g_message("Allocate error!");
		ret = -1;
		goto out;
	}
    int i=0;
    int offset=0;
    for(i = 0; i < g_list_length(snapshot_list); i++){
        struct snapshot *ss = g_list_nth_data(snapshot_list,i);
        memcpy(snapshots_buf+offset,ss,sizeof(struct snapshot));
        offset +=sizeof(struct snapshot);
    }
    *num_entries = g_list_length(snapshot_list);
out: 
	g_free(storage);
	g_hash_table_destroy(ss_hashtable);
    if(snapshot_list!=NULL){
        g_list_free(snapshot_list);
    }
	HLOG_DEBUG("leave func %s", __func__);
	return (struct snapshot *) snapshots_buf;
}
