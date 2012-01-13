/*
 *  src/snapshot/hlfs_rm_snapshot.c
 *
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

#if 0
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
#endif

int renew_tree_snapshots(struct back_storage *storage, const char *sname)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	int i = 0;
	if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
		HLOG_ERROR("snapshot.txt is not exist");
		ret = -1;
		goto out1;
	}
	bs_file_info_t *file_info = storage->bs_file_info(storage, SNAPSHOT_FILE);
	if (NULL == file_info) {
		HLOG_ERROR("get snapshot info error!");
		ret = -1;
		goto out1;
	}
	uint32_t file_size = file_info->size; 
	g_free(file_info);
	HLOG_DEBUG("file_size : %u", file_size);
	char *buf = (char *)g_malloc0(sizeof(char) * file_size);
	if (NULL == buf) {
		HLOG_ERROR("Allocate error!");
		ret = -1;
		goto out1;
	}
	bs_file_t file = NULL;
	file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_READONLY);
	if (file == NULL) {
		HLOG_ERROR("open snapshot.txt error");
		ret = -2;
		goto out;
	}
//	g_mutex_lock(ctrl->hlfs_access_mutex);
	ret = storage->bs_file_pread(storage, file, buf, file_size, 0);
//	g_mutex_unlock(ctrl->hlfs_access_mutex);
	if (ret < 0) {
		HLOG_ERROR("Read file snapshot.txt failed\n");
		storage->bs_file_close(storage, file);
		ret = -3;
		goto out;
	}
	storage->bs_file_close(storage, file);

	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	gchar **sss = g_strsplit(buf, "\n", 0);
	HLOG_DEBUG("g strv length:%d:", g_strv_length(sss));
	struct snapshot tmp_ss;
	memset(&tmp_ss, 0, sizeof(struct snapshot));
	for (i = 0; i < g_strv_length(sss) - 1; i++) {
		int flag = -1;
		struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
		if (NULL == ss) {
			HLOG_ERROR("Allocate error!");
			g_hash_table_destroy(ss_hashtable);
			ret = -4;
			goto out2;
		}
		load_ss_from_text(ss, sss[i], &flag);
		if (0 == g_strcmp0(sname, ss->sname)) {
			memcpy(&tmp_ss, ss, sizeof(struct snapshot));
		}
		if (0 == g_strcmp0(ss->up_sname, tmp_ss.sname)) {
			memset(ss->up_sname, HLFS_FILE_NAME_MAX, 0);
			snprintf(ss->up_sname, HLFS_FILE_NAME_MAX, "%s", tmp_ss.up_sname);
		}
		if (flag == 0) {
			g_hash_table_insert(ss_hashtable, ss->sname, ss);
			HLOG_DEBUG("insert snapshot [%s]", ss->sname);
		} else if (flag == 1) {
			HLOG_DEBUG("remove snapshot [%s]", ss->sname);
			if (TRUE != g_hash_table_remove(ss_hashtable, ss->sname)) {
				HLOG_ERROR("snapshot [%s] remove from hash table error!", ss->sname);
				g_free(ss);
				g_hash_table_destroy(ss_hashtable);
				ret = -5;
				goto out2;
			}
			HLOG_DEBUG("remove snapshot [%s]", ss->sname);
			g_free(ss);
			continue;
		} else {
			HLOG_ERROR("error - flag");
			g_free(ss);
			g_hash_table_destroy(ss_hashtable);
			ret = -6;
			goto out2;
		}
	}
	if (0 > rewrite_snapshot_file(storage, ss_hashtable)) {
		HLOG_ERROR("rewrite snapshot file error!!!");
		ret = -1;
	}
	g_hash_table_destroy(ss_hashtable);
out2:
	g_strfreev(sss);
out:
	g_free(buf);
out1:
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

#if 0
static int
renew_tree_snapshots(struct back_storage *storage,
			const char *sname) {
	GHashTable *shash = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	int ret = load_all_ss(storage, shash);
	if (0 > ret) {
		HLOG_ERROR("load all ss error!");
		g_hash_table_destroy(shash);
		return -1;
	}
	struct snapshot *ss = g_hash_table_lookup(shash, sname);
	if (NULL == ss) {
		HLOG_ERROR("look up ss error!");
		g_hash_table_destroy(shash);
		return -1;
	}
	GList *list = g_hash_table_get_values(shash);
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
	for (i = 0; i < len; i++) {
		struct snapshot *_ss = g_list_nth_data(list, i);
		if (0 == g_strcmp0(_ss->up_sname, ss->sname)) {
			memset(_ss->up_sname, HLFS_FILE_NAME_MAX, 0);
			snprintf(_ss->up_sname, HLFS_FILE_NAME_MAX, "%s", ss->up_sname);
		}
	}
	if (0 > rewrite_snapshot_file(storage, shash)) {
		HLOG_ERROR("rewrite snapshot file error!!!");
		g_hash_table_destroy(shash);
		g_free(list);
		return -1;
	}
	g_hash_table_destroy(shash);
	g_free(list);
	return 0;
}
#endif

int 
hlfs_rm_snapshot(const char *uri,const char *ssname) {
    int ret = 0;
    bs_file_t file = NULL;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("storage init error!");
        return -1;
    }
    if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
        HLOG_DEBUG("snapshot file not exist");
#if 0
        file = storage->bs_file_create(storage, SNAPSHOT_FILE);
        if (NULL == file) {
			HLOG_ERROR("can not create snapshot del file!");
			goto out;
		}
		storage->bs_file_close(storage, file);
#endif
		ret = -1;
		goto out;
	}
	if (1 == (ret = is_sname_exist(storage, ssname))) {
		HLOG_DEBUG("snapshot %s is not exist, right???", ssname);
		goto out;
	} else if (-1 == ret) {
		HLOG_ERROR("is sname exist error!");
		goto out;
	}
	if (0 > renew_tree_snapshots(storage, ssname)) {
		HLOG_DEBUG("renew tree snapshot error!!!");
		goto out;
	}
	file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open snapshot file");
		goto out;
	}
    char deltext[128];
	memset(deltext, 0, 128);
	uint32_t len = snapshot_delmark2text(ssname, deltext);
	HLOG_DEBUG("delbuf is %s", deltext);
//	g_mutex_lock(ctrl->hlfs_access_mutex);
	if (0 > storage->bs_file_append(storage, file, deltext, len)) {
		HLOG_ERROR("append del text failed!");
//		g_mutex_unlock(ctrl->hlfs_access_mutex);
		ret = -1;
	}
//	g_mutex_unlock(ctrl->hlfs_access_mutex);
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
    HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
