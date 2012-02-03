#include <stdio.h>
#include <string.h>
#include "snapshot.h"
#include "hlfs_log.h"

int append_ss_delmark(struct back_storage *storage, const char *ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	char *buf = (char *)g_malloc0(strlen(ss_name) + 22);
	gint size = 0;
	size = g_sprintf(buf, "-%s#null#null#null#null\n", ss_name);
	HLOG_DEBUG("size: %d", size);

	HLOG_DEBUG("enter func %s", __func__);
	HLOG_DEBUG("size will be written to the file:%d", size);

	bs_file_t file = NULL;
#if 0
/*if delmark file is not exist, create it*/
	if (-1 == storage->bs_file_is_exist(storage, file_name)) {
		g_message("ss_delmark.txt is not exist...creat it now");
		file = storage->bs_file_create(storage, file_name);
		if (file == NULL) {
			HLOG_ERROR("Create ss_delmark.txt failed");
			g_message("Create ss_delmark.txt failed");
			g_free(buf);
			return -1;
		}
		storage->bs_file_close(storage, file);
	}
#endif

	file = storage->bs_file_open(storage, SS_FILE, BS_WRITEABLE);
	if (file == NULL) {
		HLOG_ERROR("Open snapshot.txt failed");
		g_free(buf);
		return -2;
	}
	
	ret = storage->bs_file_append(storage, file, buf, size);

	if (ret < 0) {
		HLOG_ERROR("append delmark buf failed");
		storage->bs_file_close(storage, file);
		g_free(buf);
		return -3;
	}

	HLOG_DEBUG("append delmark buf successfully");
	HLOG_DEBUG("append size: %d", ret);
	
	storage->bs_file_close(storage, file);
	g_free(buf);
	HLOG_DEBUG("leave func %s successfully", __func__);
	return 0;
}

int ss2text(struct snapshot *ss, char *buf, int flag)
{
	HLOG_DEBUG("enter func %s", __func__);
	memset(buf, 0, sizeof(*buf));
	if (flag == 0) {
		sprintf(buf, "+%s#%s#%llu#%llu#%llu\n",	ss->ss_name, \
				ss->up_ss_name, ss->version, ss->ime.inode_no, \
				ss->ime.inode_addr);
	} else if (flag == 1) {
		sprintf(buf, "-%s#%s#%llu#%llu#%llu\n",	ss->ss_name, \
				ss->up_ss_name, ss->version, ss->ime.inode_no, \
				ss->ime.inode_addr);
	
	} else {
		HLOG_ERROR("flag input error");
		return -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int dump_ss_text(struct back_storage *storage, const char *buf)
{
	HLOG_DEBUG("enter func %s", __func__);
	bs_file_t file = NULL;
	int ret = 0;

	if (-1 == storage->bs_file_is_exist(storage, SS_FILE)) {
		g_message("snapshot.txt is not exist...creat it now");
		file = storage->bs_file_create(storage, SS_FILE);
		if (file == NULL) {
			HLOG_ERROR("Create snapshot.txt failed");
			g_message("Create snapshot.txt failed");
			return -1;
		}
		storage->bs_file_close(storage, file);
	}

	file = storage->bs_file_open(storage, SS_FILE, BS_WRITEABLE);

	if (file == NULL) {
		HLOG_ERROR("open file snapshot.txt failed");
		g_message("open file snapshot.txt failed");
		return -2;
	}

#if 0
	g_message("%s", buf);
	g_message("%d", strlen(buf));
#endif
	
	if (0 > (ret = storage->bs_file_append(storage, file, buf, strlen(buf)))) {
		HLOG_ERROR("write seg usage");
#if 0
		g_message("write seg error");
#endif
		ret = -3;
		storage->bs_file_close(storage, file);
		return ret;
	}

	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int dump_ss(struct back_storage *storage, struct snapshot *ss, int flag)
{
	HLOG_DEBUG("enter func %s", __func__);
	char buf[sizeof(struct snapshot) + 6];
	int ret = 0;
	ss2text(ss, buf, flag);
#if 0
	g_message("%s", buf);
#endif
	if (0 > (ret = dump_ss_text(storage, buf))) {
		HLOG_ERROR("dump_ss_text error");
		return ret;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int load_ss_from_text(struct snapshot *ss, const char *buf, int *flag)
{
	HLOG_DEBUG("enter func %s", __func__);
	gchar **v = g_strsplit(buf, "#", 1024);
	gchar *_ss_name = v[0] + 1;
	gchar *_up_ss_name = v[1];
	gchar *_version = v[2];
	gchar *_ime_inode_no = v[3];
	gchar *_ime_inode_addr = v[4];
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
	ss->version = strtoull(_version, &endptr, 0);
	sprintf(ss->ss_name, "%s", _ss_name);
	sprintf(ss->up_ss_name, "%s", _up_ss_name);
	ss->ime.inode_no = strtoull(_ime_inode_no, &endptr, 0);
	ss->ime.inode_addr = strtoull(_ime_inode_addr, &endptr, 0);

	HLOG_DEBUG("ss->version:%llu  ss->ss_name:%s  ss->up_ss_name:%s", \
			ss->version, ss->ss_name, ss->up_ss_name);
	HLOG_DEBUG("ss->inode_no:%llu  ss->inode_addr:%llu", ss->ime.inode_no, \
			ss->ime.inode_addr);
#if 0
	g_message("ss->version:%llu  ss->ss_name:%s  ss->up_ss_name:%s", \
			ss->version, ss->ss_name, ss->up_ss_name);
	g_message("ss->inode_no:%llu  ss->inode_addr:%llu", ss->ime.inode_no, \
			ss->ime.inode_addr);
#endif
	g_strfreev(v);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int load_all_ss(struct back_storage *storage, GHashTable *ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	int i;

	if (-1 == storage->bs_file_is_exist(storage, SS_FILE)) {
		HLOG_ERROR("snapshot.txt is not exist");
		return -1;
	}

	bs_file_info_t *file_info = storage->bs_file_info(storage, SS_FILE);
	uint32_t file_size = file_info->size; 
	g_free(file_info);
	HLOG_DEBUG("file_size : %ld", file_size);
	char buf[file_size];
	memset(buf, 0, file_size);
	int ret = 0;

	bs_file_t file = storage->bs_file_open(storage, SS_FILE, BS_READONLY);

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
			g_hash_table_insert(ss_hashtable, ss->ss_name, ss);
			HLOG_DEBUG("insert --------------%s", ss->ss_name);
		} else if (flag == 1) {
			g_hash_table_remove(ss_hashtable, ss->ss_name);
			HLOG_DEBUG("remove --------------%s", ss->ss_name);
			g_free(ss);
			continue;
		} else {
			HLOG_DEBUG("error - flag");
			return -4;
		}
	}
	g_strfreev(sss);
	storage->bs_file_close(storage, SS_FILE); 

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
	ss->version = _ss->version;
	sprintf(ss->ss_name, "%s", _ss->ss_name);
	sprintf(ss->up_ss_name, "%s", _ss->up_ss_name);
	ss->ime.inode_no = _ss->ime.inode_no;
	ss->ime.inode_addr = _ss->ime.inode_addr;
#if 0
	g_message("seg fault test");
#endif
#if 0
	g_message("%llu\n%s\n%s\n%llu\n%llu", _ss->version, _ss->ss_name, \
			_ss->up_ss_name, _ss->ime.inode_no, _ss->ime.inode_addr);
#endif
#if 0
	g_message("seg fault test");
#endif
	g_hash_table_destroy(ss_hashtable);
	return 0;
}
