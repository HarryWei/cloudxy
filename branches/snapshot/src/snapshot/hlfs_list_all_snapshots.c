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
 * list_key() will be the parameter of the function g_list_foreach() which will 
 * be invoked by hlfs_list_all_snapshots().
 */
void list_key(gpointer data, gpointer usr_data)
{
	HLOG_DEBUG("enter func %s", __func__);
	int size = 0;
	if (data == NULL) {
		HLOG_DEBUG("data is NULL");
		return ;
	}
	char tmp[128];
#if 0
	tmp = (char *)g_malloc0(128);
	if (NULL == tmp) {
		HLOG_ERROR("Allocate error!");
		return ;
	}
#endif
	memset(tmp, 0, 128);
	sprintf(tmp, "%s\n", (char *)data);
	size = g_strlcat(usr_data, tmp, MAX_BUFSIZE);
	if (MAX_BUFSIZE < size) {
		HLOG_ERROR("MAX_BUFSIZE is not enough");
		return;
	}
	HLOG_DEBUG("leave func %s", __func__);
}

/*
 * hlfs_list_all_snapshots is one of snapshot module's APIs.It's function is 
 * loading all snapshot names to the memery.
 */
int hlfs_list_all_snapshots(const char *uri, char **ss_name_array)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
#if 0
	char *tmp_buf = NULL;
	tmp_buf = (char *)g_malloc0(MAX_BUFSIZE);
	if (NULL == tmp_buf) { //TODO if size of snapshot.txt > MAX_BUFSIZE
		HLOG_ERROR("Allocate error!");
		ret = -1;
		goto out;
	}
	*ss_name_array = tmp_buf;
#endif
	*ss_name_array = (char *)g_malloc0(MAX_BUFSIZE);
	if (NULL == *ss_name_array) { //TODO if size of snapshot.txt > MAX_BUFSIZE
		HLOG_ERROR("Allocate error!");
		ret = -1;
		goto out;
	}
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		ret = -1;
		goto out;
	}
	ret = load_all_ss(storage, ss_hashtable);
	if (ret < 0) {
		HLOG_ERROR("load all ss error: %d", ret);
		ret = -1;
		goto out;
	}
	if (0 == g_hash_table_size(ss_hashtable)) {
		HLOG_DEBUG("We may have not taken snapshot yet!!!");
		g_free(*ss_name_array);
		*ss_name_array = NULL;
		ret = 1;
		goto out;
	}
	GList *list = g_hash_table_get_keys(ss_hashtable);
//	ret = load_all_ss(storage, ss_hashtable);
	g_list_foreach(list, list_key, *ss_name_array);
	HLOG_DEBUG("buf:%s", *ss_name_array);
	if (0 > rewrite_snapshot_file(storage, ss_hashtable)) {
		HLOG_ERROR("rewrite snapshot.txt error");
		ret = -1;
		goto out;
	}
	g_hash_table_destroy(ss_hashtable);
	if (*ss_name_array == NULL) {
		HLOG_DEBUG("Buf is NULL after listing");
		ret = -1;
		goto out;
	}
out:
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
