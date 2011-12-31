#include <stdint.h>
#include <string.h>
#include <hlfs_ctrl.h>
#include <stdio.h>
#include "hlfs_log.h"
#include "snapshot.h"

int snapshot2text(const struct snapshot *snapshot, char *textbuf) {
	memset(textbuf, 0, strlen(textbuf));
	int n = sprintf(textbuf, "+%s@@##$$%llu@@##$$%llu@@##$$%s\n", snapshot->sname, \
			snapshot->timestamp, snapshot->inode_addr, snapshot->up_sname);
	return n;
}

int dump_snapshot(struct back_storage *storage, const char *snapshot_file, \
		struct snapshot *snapshot) {
    if(snapshot_file == NULL || snapshot == NULL || storage == NULL){
        return -1;
    }
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;
	if (-1 == storage->bs_file_is_exist(storage, snapshot_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = storage->bs_file_create(storage, snapshot_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s", snapshot_file);
			goto out1;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, snapshot_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", snapshot_file);
		goto out1;
	}
    char snapshot_text[1024];
    memset(snapshot_text, 0, 1024);
	len = snapshot2text(snapshot, snapshot_text);
	HLOG_DEBUG("ss text is %s", snapshot_text);
	if (len !=  storage->bs_file_append(storage, file, snapshot_text, len)) {
		HLOG_ERROR("write ss file error, write bytes %d", ret);
		ret = -1;
		goto out1;
	}
out1:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	return ret;
}

int snapshot_delmark2text(const char *ssname, char *textbuf) {
	memset(textbuf, 0, 128);
	int n = sprintf(textbuf, "-%s@@##$$\n", ssname);
	return n;
}

int dump_snapshot_delmark(struct back_storage *storage, const char *snapshot_file, \
		const char *ssname){
    if(snapshot_file == NULL || ssname == NULL || storage == NULL){
        return -1;
    }
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;
	if (-1 == storage->bs_file_is_exist(storage, snapshot_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = storage->bs_file_create(storage,snapshot_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s", snapshot_file);
			goto out2;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, snapshot_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open ss file %s", snapshot_file);
		goto out2;
	}
    char snapshot_delmark_text[1024];
    memset(snapshot_delmark_text, 0, 1024);
	len = snapshot_delmark2text(ssname, snapshot_delmark_text);
	HLOG_DEBUG("cp text is %s", snapshot_delmark_text);
	if (len != storage->bs_file_append(storage, file, snapshot_delmark_text, len)) {
		HLOG_ERROR("write cp file error, write bytes %d", ret);
		ret = -1;
		goto out2;
	}
out2:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	return ret;
}

int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag)
{
    gchar **v = g_strsplit(buf, "@@##$$", 2);
    if ('+' == v[0][0]) {
        *flag = 0;
		gchar **_v = g_strsplit(v[1], "@@##$$", 3);
    	gchar *_ss_name = v[0] + 1;
    	gchar *_version = _v[0];
		gchar *_ime_inode_addr = _v[1];
    	gchar *_up_ss_name = _v[2];
		if (((strlen(_ss_name) + 1) > HLFS_FILE_NAME_MAX) ||
				(strlen(_up_ss_name) + 1) > HLFS_FILE_NAME_MAX) {
			HLOG_ERROR("snapshot name or up snapshot name beyond max length!");
			return -1;
		}
    	char *endptr = NULL;
		ss->timestamp = strtoull(_version, &endptr, 0); 
    	sprintf(ss->sname, "%s", _ss_name);
		sprintf(ss->up_sname, "%s", _up_ss_name);
		ss->inode_addr = strtoull(_ime_inode_addr, &endptr, 0); 
		HLOG_DEBUG("ss->timestamp:%llu  ss->sname:%s  ss->up_sname:%s, \
					ss->inode_addr: %llu", ss->timestamp, ss->sname, 
					ss->up_sname, ss->inode_addr);
		g_strfreev(_v);
    } else if ('-' == v[0][0]) {
        *flag = 1;
    	sprintf(ss->sname, "%s", v[0] + 1);
		sprintf(ss->up_sname, "%s", "removed");
		ss->timestamp = 0;
		ss->inode_addr = 0;
    } else {
		HLOG_ERROR("flag parse error");
		g_strfreev(v);
		return -1; 
	}   
	g_strfreev(v);
	return 0;
}

int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable)
{
	int ret = 0;
	int i = 0;
	g_message("%s -- 77 dbg", __func__);
	if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
		HLOG_ERROR("snapshot.txt is not exist");
		ret = -1;
		return ret;
	}
	bs_file_info_t *file_info = storage->bs_file_info(storage, SNAPSHOT_FILE);
	if (NULL == file_info) {
		HLOG_ERROR("get snapshot info error!");
		ret = -1;
		return ret;
	}
	uint32_t file_size = file_info->size; 
	g_free(file_info);
	HLOG_DEBUG("file_size : %u", file_size);
	char buf[file_size];
	memset(buf, 0, file_size);
	bs_file_t file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_READONLY);
	if (file == NULL) {
		HLOG_ERROR("open snapshot.txt error");
		ret = -2;
		return ret;
	}
	ret = storage->bs_file_pread(storage, file, buf, file_size, 0);
	if (ret < 0) {
		HLOG_ERROR("Read file snapshot.txt failed\n");
		if (NULL != file) {
			storage->bs_file_close(storage, file);
		}
		ret = -3;
		return ret;
	}
	gchar **sss = g_strsplit(buf, "\n", 0);
	g_message("g strv length:%d:", g_strv_length(sss));
	for (i = 0; i < g_strv_length(sss) - 1; i++) {
		int flag = -1;
		struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
		if (NULL == ss) {
			g_message("Allocate error!");
			if (NULL != file) {
				storage->bs_file_close(storage, file);
			}
			ret = -1;
			return ret;
		}
		g_message("7771 dbg");
		load_ss_from_text(ss, sss[i], &flag);
		g_message("7772 dbg");
		if (flag == 0) {
			g_hash_table_insert(ss_hashtable, ss->sname, ss);
			g_message("insert snapshot [%s]", ss->sname);
		} else if (flag == 1) {
			g_message("remove snapshot [%s]", ss->sname);
			if (TRUE != g_hash_table_remove(ss_hashtable, ss->sname)) {
				g_message("snapshot [%s] remove from hash table error!", ss->sname);
				return -1;
			}
			g_message("remove snapshot [%s]", ss->sname);
			g_free(ss);
			continue;
		} else {
			g_message("error - flag");
			g_strfreev(sss);
			if (NULL != file) {
				storage->bs_file_close(storage, file);
			}
			ret = -4;
			return ret;
		}
	}
	g_strfreev(sss);
	storage->bs_file_close(storage, SNAPSHOT_FILE); 
#if 0
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
#endif
	return 0;
}

int load_ss_by_name(struct back_storage *storage, 
					struct snapshot *ss, 
					const char *ss_name)
{
	int res = 0;
	struct snapshot *_ss = NULL;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	res = load_all_ss(storage, ss_hashtable);
	if (res < 0) {
		HLOG_ERROR("load all ss error");
		return -1;
	}
	_ss = g_hash_table_lookup(ss_hashtable, ss_name);
	if (NULL == _ss) {
		HLOG_DEBUG("No such key in table");
		return 1;
	}
	ss->timestamp = _ss->timestamp;
	sprintf(ss->sname, "%s", _ss->sname);
	sprintf(ss->up_sname, "%s", _ss->up_sname);
	ss->inode_addr = _ss->inode_addr;
	g_hash_table_destroy(ss_hashtable);
	return 0;
}

void dump_ss_one_by_one(gpointer data, gpointer storage)
{
	if (NULL == data || NULL == storage) {
		HLOG_ERROR("Param error");
		return;
	}
	struct snapshot *ss = (struct snapshot *)data;
	char *file_name = SNAPSHOT_FILE;
	if (0 > dump_snapshot((struct back_storage *)storage, file_name, ss)) {
		HLOG_ERROR("dump ss error");
		return;
	}
}

int rewrite_snapshot_file(struct back_storage *storage, GHashTable *ss_hashtable)
{
	if((0 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) && 
			(0 > storage->bs_file_delete(storage, SNAPSHOT_FILE))) {
		HLOG_ERROR("remove snapshot.txt failed");
		return -1;
	}
	bs_file_t file = storage->bs_file_create(storage, SNAPSHOT_FILE);
	if (file == NULL) {
		HLOG_ERROR("create snapshot.txt error");
		return -2;
	}
	storage->bs_file_close(storage, file);
	GList *list = g_hash_table_get_values(ss_hashtable);
	g_list_foreach(list, dump_ss_one_by_one, storage);
	g_list_free(list);
	return 0;
}
