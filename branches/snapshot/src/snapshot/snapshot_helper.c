#include <stdint.h>
#include <hlfs_ctrl.h>
#include <stdio.h>
#include "hlfs_log.h"
#include "snapshot.h"

int snapshot2text(const struct snapshot *snapshot, char *textbuf) {
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	memset(textbuf, 0, sizeof(struct snapshot) * 2);
	int n = sprintf(textbuf, "%s%s#%llu#%llu#%s\n", "+", snapshot->sname, \
			snapshot->timestamp, snapshot->inode_addr, snapshot->up_sname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}

int dump_snapshot(struct back_storage *storage, const char *snapshot_file, \
		struct snapshot *snapshot) {
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
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
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}

int snapshot_delmark2text(const char *ssname, char *textbuf) {
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	memset(textbuf, 0, sizeof(struct snapshot) * 2);
	int n = sprintf(textbuf, "%s%s###\n", "-", ssname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}

int dump_snapshot_delmark(struct back_storage *storage, const char *snapshot_file, \
		const char *ssname){
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
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
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, snapshot_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open ss file %s", snapshot_file);
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
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}

int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag)
{
	HLOG_DEBUG("enter func %s", __func__);
    gchar **v = g_strsplit(buf, "#", 1024);
    gchar *_ss_name = v[0] + 1;
    gchar *_version = v[1];
	gchar *_ime_inode_addr = v[2];
    gchar *_up_ss_name = v[3];
    if ('+' == v[0][0]) {
        *flag = 0;
    } else if ('-' == v[0][0]) {
        *flag = 1;
    } else {
		HLOG_ERROR("flag parse error");
		g_strfreev(v);
		return -1; 
	}   
    char *endptr = NULL;
	ss->timestamp = strtoull(_version, &endptr, 0); 
    sprintf(ss->sname, "%s", _ss_name);
	sprintf(ss->up_sname, "%s", _up_ss_name);
	ss->inode_addr = strtoull(_ime_inode_addr, &endptr, 0); 

	HLOG_DEBUG("ss->timestamp:%llu  ss->sname:%s  ss->up_sname:%s, \
			ss->inode_addr: %llu", ss->timestamp, ss->sname, ss->up_sname, \
			ss->inode_addr);
	g_strfreev(v);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	int i;
	if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
		HLOG_ERROR("snapshot.txt is not exist");
		return -1;
	}

	bs_file_info_t *file_info = storage->bs_file_info(storage, SNAPSHOT_FILE);
	uint32_t file_size = file_info->size; 
	g_free(file_info);
	HLOG_DEBUG("file_size : %u", file_size);
	char buf[file_size];
	memset(buf, 0, file_size);
	int ret = 0;

	bs_file_t file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_READONLY);

	if (file == NULL) {
		HLOG_ERROR("open snapshot.txt error");
		return -2;
	}
	
	ret = storage->bs_file_pread(storage, file, buf, file_size, 0);
	if (ret < 0) {
		g_message("Read file snapshot.txt failed\n");
		HLOG_ERROR("Read file snapshot.txt failed\n");
		storage->bs_file_close(storage, file);
		return -3;
	}
#if 0
	g_message("%s", buf);
	g_message("finished");
#endif

	gchar **sss = g_strsplit(buf, "\n", 1024);
#if 0
	while (*sss != NULL) {
		g_message("%s", *sss);
		*sss++;
	}
#endif
	HLOG_DEBUG("g strv length:%d:", g_strv_length(sss));
	for (i = 0; i < g_strv_length(sss) - 1; i++) {
		int flag = -1;
		struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
		load_ss_from_text(ss, sss[i], &flag);
		if (flag == 0) {
			g_hash_table_insert(ss_hashtable, ss->sname, ss);
			HLOG_DEBUG("insert --------------%s", ss->sname);
		} else if (flag == 1) {
			g_hash_table_remove(ss_hashtable, ss->sname);
			HLOG_DEBUG("remove --------------%s", ss->sname);
			g_free(ss);
			continue;
		} else {
			HLOG_DEBUG("error - flag");
			return -4;
		}
	}
	g_strfreev(sss);
	storage->bs_file_close(storage, SNAPSHOT_FILE); 

	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int load_ss_by_name(struct back_storage *storage, struct snapshot *ss, \
		const char *ss_name)
{
	int res = 0;
	struct snapshot *_ss;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, \
			g_str_equal, NULL, NULL);
	res = load_all_ss(storage, ss_hashtable);
#if 0 
	g_message("%d", ret);
#endif
	if (res < 0) {
		HLOG_ERROR("load all ss error");
		return -1;
	}
#if 0
	g_message("seg fault test");
#endif
	_ss = g_hash_table_lookup(ss_hashtable, ss_name);
	if (NULL == _ss) {
		HLOG_DEBUG("No such key in table");
		return -2;
	}
	ss->timestamp = _ss->timestamp;
	sprintf(ss->sname, "%s", _ss->sname);
	sprintf(ss->up_sname, "%s", _ss->up_sname);
	ss->inode_addr = _ss->inode_addr;
#if 0
	g_message("seg fault test");
#endif
#if 0
	g_message("%llu\n%s\n%s\n%llu\n%llu", _ss->version, _ss->sname, \
			_ss->up_sname, _ss->ime.inode_no, _ss->ime.inode_addr);
#endif
#if 0
	g_message("seg fault test");
#endif
	g_hash_table_destroy(ss_hashtable);
	return 0;
}

