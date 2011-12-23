/*
 *  src/snapshot/hlfs_list_all_snapshots.c
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
static int snapshot_delmark2text(const char *ssname, char *deltext) {
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	int n = sprintf(deltext, "%s\n", ssname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}
#endif

int 
hlfs_list_all_snapshots(const char *uri, 
						char **ssname) {
	g_message("enter func %s", __func__);
	int ret = 0;
	int i = 0;
	bs_file_t file = NULL;

	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		g_message("storage init error!");
		ret = -1;
		goto out;
	}
	if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_USAGE_FILE)) {
		g_message("snapshot del file not exist");
		ret = -1;
		goto out;
	}
	file = storage->bs_file_open(storage, SNAPSHOT_USAGE_FILE, BS_READONLY);
	if (NULL == file) {
		g_message("can not open snapshot usage file");
		ret = -1;
		goto out;
	}
	bs_file_info_t *info = NULL;
	if (NULL == (info = storage->bs_file_info(storage, SNAPSHOT_USAGE_FILE))) {
		g_message("get file info error!");
		ret = -1;
		goto out;
	}
	char *tmp_buf = (char *)g_malloc0(sizeof(char) * info->size);
	if (NULL == tmp_buf) {
		g_message("allocate error!");
		ret = -1;
		goto out;
	}
	uint32_t size = storage->bs_file_pread(storage, file, tmp_buf, info->size, 0);
	if (size != info->size) {
		g_message("read snapshot usage file error: %d", size);
		ret = -1;
		goto out;
	}
	g_message("dbg 77 size is %d", size);
	gchar **v = NULL;
	gchar **v1 = NULL;
	gchar textbuf[SNAME_LEN];
	memset(textbuf, 0, SNAME_LEN);
	v = g_strsplit(tmp_buf, "\n", 0);
	*ssname = (char *)g_malloc0(sizeof(char) * info->size);
	for (i = 0; i < g_strv_length(v) - 1; i++) {
		v1 = g_strsplit(v[i], " ", 3);
		g_message("ssname is %s", v1[2]);
		sprintf(textbuf, "%s ", v1[2]);
		g_strlcat(*ssname, textbuf, info->size);
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	g_strfreev(v);
	g_strfreev(v1);
	return ret;
}
