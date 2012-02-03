/*
 *  src/snapshot/hlfs_rm_snapshot.c
 
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"

static int 
is_sname_exist(struct back_storage *storage,
				const char *sname) {
	GHashTable *shash = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	int ret = load_all_ss(storage, shash);
	if (0 > ret) {
		HLOG_ERROR("load all ss error!");
		g_hash_table_destroy(shash);
		return -1;
	}
	if (NULL == g_hash_table_lookup(shash, sname)) {
		HLOG_DEBUG("we can not find %s in the hash table!", sname);
		g_hash_table_destroy(shash);
		return 1;
	}
	g_hash_table_destroy(shash);
	return 0;
}

static int
is_delete(struct back_storage *storage,
			const char *sname) {
	GHashTable *shash = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	int ret = load_all_ss(storage, shash);
	if (0 > ret) {
		HLOG_ERROR("load all ss error!");
		g_hash_table_destroy(shash);
		return -1;
	}
#if 0
	struct snapshot *ss = g_hash_take_lookup(shash, sname);
	if (NULL == ss) {
		HLOG_ERROR("look up ss error!");
		g_hash_table_destroy(shash);
		return -1;
	}
#endif
	Glist *list = g_hash_table_get_values(shash) ;
	if (NULL == list) {
		HLOG_ERROR("hash table get values error!");
		g_hash_table_destroy(shash);
		return -1;
	}
	int len = g_list_length(list);
	if (0 == len) {
		HLOG_ERROR("There is no snapshots, right?");
		g_hash_table_destroy(shash);
		g_free(list);
		return -1;
	}
	int i = 0;
	for (i = 0; i < g_list_length(list); i++) {
		struct snapshot *_ss = g_list_nth_data(list, i);
		if (0 == g_strcmp0(_ss->up_sname, sname)) {
			return 1; // can not delete it
		}
	}
	return 0; // can delete it
}

int 
hlfs_rm_snapshot(const char *uri,const char *ssname) {
    int ret = 0;
    bs_file_t file = NULL;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("storage init error!");
        return -1;
    }
	if (1 == (ret = is_sname_exist(storage, ssname))) {
		HLOG_DEBUG("snapshot %s is not exist, right???", ssname);
		goto out;
	} else if (-1 == ret) {
		HLOG_ERROR("is sname exist error!");
		goto out;
	}
	if (1 == (ret = is_delete(storage, ssname))) {
		HLOG_DEBUG("snapshot %s can not be deleted", ssname);
		goto out;
	} else if (-1 == ret) {
		HLOG_ERROR("is_delete executed error!");
		goto out;
	}
    if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
        HLOG_DEBUG("snapshot del file not exist, create it");
        file = storage->bs_file_create(storage, SNAPSHOT_FILE);
        if (NULL == file) {
			HLOG_ERROR("can not create snapshot del file!");
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open snapshot del file");
		goto out;
	}
    char deltext[128];
	memset(deltext, 0, 128);
	uint32_t len = snapshot_delmark2text(ssname, deltext);
	HLOG_DEBUG("delbuf is %s", deltext);
	g_mutex_lock(ctrl->hlfs_access_mutex);
	if (0 > storage->bs_file_append(storage, file, deltext, len)) {
		HLOG_ERROR("append del text failed!");
		g_mutex_unlock(ctrl->hlfs_access_mutex);
		ret = -1;
	}
	g_mutex_unlock(ctrl->hlfs_access_mutex);
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
    HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
