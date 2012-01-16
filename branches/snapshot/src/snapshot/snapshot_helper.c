#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include "hlfs_ctrl.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "snapshot.h"
#include "misc.h"
#include "comm_define.h"

int is_sname_exist(struct back_storage *storage,
				const char *sname) {
	if (EHLFS_NOFILE == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
		HLOG_DEBUG("There is no snapshot file!");
		return 1;
	}
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

/* we can not find the inode_addr's snapshot name, also the up snapshot name*/
int create_auto_snapshot(struct hlfs_ctrl *ctrl, uint64_t inode_addr)
{
	HLOG_DEBUG("enter func %s", __func__);
	struct snapshot ss;
	memset(&ss, 0, sizeof(struct snapshot));
	sprintf(ss.sname, "%llu", inode_addr);
	sprintf(ss.up_sname, "%llu", inode_addr);
	ss.inode_addr = inode_addr;
	ss.timestamp = get_current_time();
	g_mutex_lock(ctrl->hlfs_access_mutex);
	dump_snapshot(ctrl->storage, SNAPSHOT_FILE, &ss);
	g_mutex_unlock(ctrl->hlfs_access_mutex);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int create_adam_snapshot(struct hlfs_ctrl *ctrl, const char *ssname)
{
	HLOG_DEBUG("enter func %s", __func__);
	struct snapshot ss;
	memset(&ss, 0, sizeof(struct snapshot));
	sprintf(ss.sname, "%s", ssname);
	sprintf(ss.up_sname, "%s", ssname);
	ss.inode_addr = ctrl->imap_entry.inode_addr;
	ss.timestamp = get_current_time();
	g_mutex_lock(ctrl->hlfs_access_mutex);
	dump_snapshot(ctrl->storage, SNAPSHOT_FILE, &ss);
	g_mutex_unlock(ctrl->hlfs_access_mutex);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int snapshot2text(const struct snapshot *snapshot, char *textbuf) 
{
	HLOG_DEBUG("enter func %s", __func__);
	memset(textbuf, 0, strlen(textbuf));
	int n = sprintf(textbuf, "+%s@@##$$%llu@@##$$%llu@@##$$%s\n", 
					snapshot->sname,snapshot->timestamp, 
					snapshot->inode_addr, snapshot->up_sname);
	HLOG_DEBUG("leave func %s", __func__);
	return n;
}

int dump_snapshot(struct back_storage *storage, 
					const char *snapshot_file, 
					struct snapshot *snapshot) {
	HLOG_DEBUG("enter func %s", __func__);
    if(snapshot_file == NULL || snapshot == NULL || storage == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;
	if (EHLFS_NOFILE == storage->bs_file_is_exist(storage, snapshot_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = storage->bs_file_create(storage, snapshot_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s", snapshot_file);
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, snapshot_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", snapshot_file);
		goto out;
	}
    char snapshot_text[1024];
    memset(snapshot_text, 0, 1024);
	len = snapshot2text(snapshot, snapshot_text);
	HLOG_DEBUG("ss text is %s", snapshot_text);
	if (len !=  storage->bs_file_append(storage, file, snapshot_text, len)) {
		HLOG_ERROR("write ss file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int snapshot_delmark2text(const char *ssname, char *textbuf) {
	HLOG_DEBUG("enter func %s", __func__);
	memset(textbuf, 0, 128);
	int n = sprintf(textbuf, "-%s@@##$$\n", ssname);
	HLOG_DEBUG("leave func %s", __func__);
	return n;
}

int dump_snapshot_delmark(struct back_storage *storage, 
							const char *snapshot_file, 
							const char *ssname){
	HLOG_DEBUG("enter func %s", __func__);
    if (snapshot_file == NULL || ssname == NULL || storage == NULL) {
		HLOG_ERROR("Parameter Error!");
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
			ret = -1;
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, snapshot_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open ss file %s", snapshot_file);
		ret = -1;
		goto out;
	}
    char snapshot_delmark_text[1024];
    memset(snapshot_delmark_text, 0, 1024);
	len = snapshot_delmark2text(ssname, snapshot_delmark_text);
	HLOG_DEBUG("cp text is %s", snapshot_delmark_text);
	if (len != storage->bs_file_append(storage, file, snapshot_delmark_text, len)) {
		HLOG_ERROR("write cp file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag)
{
	HLOG_DEBUG("enter func %s", __func__);
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
			g_strfreev(v);
			g_strfreev(_v);
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
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	int i = 0;
	if (EHLFS_NOFILE == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
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

	gchar **sss = g_strsplit(buf, "\n", 0);
	HLOG_DEBUG("g strv length:%d:", g_strv_length(sss));
	for (i = 0; i < g_strv_length(sss) - 1; i++) {
		int flag = -1;
		struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
		if (NULL == ss) {
			HLOG_ERROR("Allocate error!");
			ret = -4;
			goto out2;
		}
		load_ss_from_text(ss, sss[i], &flag);
		if (flag == 0) {
			g_hash_table_insert(ss_hashtable, ss->sname, ss);
			HLOG_DEBUG("insert snapshot [%s]", ss->sname);
		} else if (flag == 1) {
			HLOG_DEBUG("remove snapshot [%s]", ss->sname);
			if (TRUE != g_hash_table_remove(ss_hashtable, ss->sname)) {
				HLOG_ERROR("snapshot [%s] remove from hash table error!", ss->sname);
				g_free(ss);
				ret = -5;
				goto out2;
			}
			HLOG_DEBUG("remove snapshot [%s]", ss->sname);
			g_free(ss);
			continue;
		} else {
			HLOG_ERROR("error - flag");
			g_free(ss);
			ret = -6;
			goto out2;
		}
	}
out2:
	g_strfreev(sss);
out:
	g_free(buf);
out1:
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int load_ss_by_name(struct back_storage *storage, 
					struct snapshot *ss, 
					const char *ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int res = 0;
	struct snapshot *_ss = NULL;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	res = load_all_ss(storage, ss_hashtable);
	if (res < 0) {
		HLOG_ERROR("load all ss error");
		g_hash_table_destroy(ss_hashtable);
		return -1;
	}
	_ss = g_hash_table_lookup(ss_hashtable, ss_name);
	if (NULL == _ss) {
		HLOG_DEBUG("No such key in table");
		g_hash_table_destroy(ss_hashtable);
		return 1;
	}
	ss->timestamp = _ss->timestamp;
	sprintf(ss->sname, "%s", _ss->sname);
	sprintf(ss->up_sname, "%s", _ss->up_sname);
	ss->inode_addr = _ss->inode_addr;
	g_hash_table_destroy(ss_hashtable);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

void dump_ss_one_by_one(gpointer data, gpointer storage)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (NULL == data || NULL == storage) {
		HLOG_ERROR("Param error");
		return ;
	}
	struct snapshot *ss = (struct snapshot *) data;
	char *file_name = SNAPSHOT_FILE;
	if (0 > dump_snapshot((struct back_storage *)storage, file_name, ss)) {
		HLOG_ERROR("dump ss error");
		return ;
	}
	HLOG_DEBUG("leave func %s", __func__);
}

int rewrite_snapshot_file(struct back_storage *storage, GHashTable *ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
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
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

void rebuild_hashtable_use_usn_keys(gpointer data, gpointer ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (data == NULL || ss_hashtable == NULL) {
		HLOG_ERROR("param error");
		HLOG_DEBUG("leave func %s", __func__);
		return;
	}
	struct snapshot *ss = (struct snapshot *)data;
	GHashTable *ss_hash = (GHashTable *)ss_hashtable;
	g_hash_table_insert(ss_hash, ss->up_sname, ss);
	HLOG_DEBUG("leave func %s", __func__);
	return;
}

int load_all_ss_use_up_sname_keys(struct back_storage *storage, \
		GHashTable *ss_hashtable_use_up_sname_keys)
{
	HLOG_DEBUG("enter func %s", __func__);
	GHashTable *ss_hashtable_use_name_keys = g_hash_table_new_full(g_str_hash, \
			g_str_equal, NULL, NULL);
	int ret = load_all_ss(storage, ss_hashtable_use_name_keys);
	if (ret < 0) {
		HLOG_ERROR("load all ss by name error");
		g_hash_table_destroy(ss_hashtable_use_name_keys);
		return -1;
	}
	GList *list = g_hash_table_get_values(ss_hashtable_use_name_keys);
	g_list_foreach(list, rebuild_hashtable_use_usn_keys, ss_hashtable_use_up_sname_keys);
	g_list_free(list);
	g_hash_table_destroy(ss_hashtable_use_up_sname_keys);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

#if 0
void rebuild_hashtable_use_ia_keys(gpointer data, gpointer ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (data == NULL || ss_hashtable == NULL) {
		HLOG_ERROR("param error");
		HLOG_DEBUG("leave func %s", __func__);
		return;
	}
	struct snapshot *ss = (struct snapshot *)data;
	GHashTable *ss_hash = (GHashTable *)ss_hashtable;
	g_hash_table_insert(ss_hash, GINT_TO_POINTER(ss->inode_addr), ss);
	HLOG_DEBUG("leave func %s", __func__);
	return;
}

int load_all_ss_use_inode_addr_keys(struct back_storage *storage, \
		GHashTable *ss_hashtable_use_inode_addr_keys)
{
	HLOG_DEBUG("enter func %s", __func__);
	GHashTable *ss_hashtable_use_name_keys = g_hash_table_new_full(g_str_hash, \
			g_str_equal, NULL, NULL);
	int ret = load_all_ss(storage, ss_hashtable_use_name_keys);
	if (ret < 0) {
		HLOG_ERROR("load all ss by name error");
		g_hash_table_destroy(ss_hashtable_use_name_keys);
		return -1;
	}
	GList *list = g_hash_table_get_values(ss_hashtable_use_name_keys);
	if (list == NULL) {
		HLOG_ERROR("get values error");
		g_hash_table_destroy(ss_hashtable_use_name_keys);
		HLOG_DEBUG("leave func %s", __func__);
		return -1;
	}
	g_list_foreach(list, rebuild_hashtable_use_ia_keys, \
			ss_hashtable_use_inode_addr_keys);
	g_list_free(list);
	g_hash_table_destroy(ss_hashtable_use_inode_addr_keys);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

void find_up_inode_addr(gpointer data, gpointer usr_data)
{
	HLOG_DEBUG("enter func %s", __func__);
	uint64_t tmp = (uint64_t) GPOINTER_TO_INT(data);
	inode_cup_t *inode_cup = (inode_cup_t *)usr_data;
	if ((tmp < inode_cup->cur_inode_addr) && ((inode_cup->cur_inode_addr - \
					tmp) < (inode_cup->cur_inode_addr - inode_cup->up_inode_addr))) 
		inode_cup->up_inode_addr = tmp;
	HLOG_DEBUG("leave func %s", __func__);
}
#endif

int find_ss_name_of_inode(struct hlfs_ctrl *ctrl, uint64_t inode_addr, char **ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
//	inode_cup_t *inode_cup = (inode_cup_t *)g_malloc0(sizeof(inode_cup_t));
//	inode_cup->cur_inode_addr = inode_addr;
//	inode_cup->up_inode_addr = 0;
	if (EHLFS_NOFILE == (ret = ctrl->storage->bs_file_is_exist(ctrl->storage, SNAPSHOT_FILE))) {
		HLOG_DEBUG("We can not find snapshot file, create adam snapshot");
		if (0 > create_adam_snapshot(ctrl, "adam")) {
			HLOG_ERROR("create auto snapshot error!");
			g_free(*ss_name);
			return EHLFS_FUNC;
		}
		*ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
		if (NULL == *ss_name) {
			HLOG_ERROR("Allocate Error!");
			return EHLFS_MEM;
		}
		sprintf(*ss_name, "%s", "adam");
		return ret;
	} else if (EHLFS_MEM == ret) {
		HLOG_DEBUG("Invoke func error");
		return ret;
	}
	GHashTable *ss_hashtable = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, NULL);
	if (load_all_ss(ctrl->storage, ss_hashtable) < 0) {
		HLOG_ERROR("load all ss error!");
		g_hash_table_destroy(ss_hashtable);
		HLOG_DEBUG("leave func %s", __func__);
		return -1;
	}
	GList *list = g_hash_table_get_values(ss_hashtable);
	if (list == NULL) {
		HLOG_ERROR("get values error");
		g_hash_table_destroy(ss_hashtable);
		HLOG_DEBUG("leave func %s", __func__);
		return -1;
	}
#if 0
	g_list_foreach(list, find_up_inode_addr, &inode_addr);
	if (inode_cup->up_inode_addr == 0) {
		HLOG_DEBUG("No up_inode_addr exist");
		*up_ss_name = NULL;
	} else {
		*up_ss_name = (char *)g_malloc0(MAX_FILE_NAME_LEN);
		struct snapshot *tmp_ss = NULL;
		tmp_ss = g_hash_table_lookup(ss_hashtable, \
				GINT_TO_POINTER(inode_cup->up_inode_addr));
		sprintf(*up_ss_name, "%s", tmp_ss->sname);
	}
	g_free(inode_cup);
#endif
	int i = 0;
	int len = g_list_length(list);
	*ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
	if (NULL == *ss_name) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	if (0 == len) {
		HLOG_DEBUG("There is no snapshot yet, create adam snapshot");
		if (0 > create_adam_snapshot(ctrl, "adam")) {
			HLOG_ERROR("create auto snapshot error!");
			g_free(*ss_name);
			goto out;
		}
		sprintf(*ss_name, "%s", "adam");
	}
	for (i = 0; i < g_list_length(list); i++) {
		struct snapshot *ss = (struct snapshot *) g_list_nth_data(list, i);
		if (NULL == ss) {
			HLOG_ERROR("find the ss error!");
			g_free(*ss_name);
			ret = -1;
			goto out;
		}
		if (inode_addr == ss->inode_addr) {
			sprintf(*ss_name, "%s", ss->sname);
			goto out;
		}
	}
	HLOG_DEBUG("We can not find the inode_addr's snapshot, so use inode addr as up ss name");
	if (0 > create_auto_snapshot(ctrl, inode_addr)) {
		HLOG_ERROR("create auto snapshot error!");
		g_free(*ss_name);
		ret = -1;
		goto out;
	}
	sprintf(*ss_name, "%llu", inode_addr);
out:
	g_list_free(list);
	g_hash_table_destroy(ss_hashtable);
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
