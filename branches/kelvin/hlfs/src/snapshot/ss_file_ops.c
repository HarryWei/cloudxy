#include <stdio.h>
#include <string.h>
#include "snapshot.h"
#include "hlfs_log.h"

int append_ss_delmark(struct back_storage *storage, const char *ss_name)
{
	int ret = 0;
	char *file_name = SS_DEL_FILE;
	char *buf = (char *)g_malloc0(sizeof(*ss_name) + 1);
	gint size = 0;
	size = g_sprintf(buf, "%s\n", ss_name);

	HLOG_DEBUG("enter func %s", __func__);
	HLOG_DEBUG("size will be written to the file:%d", size);

	bs_file_t file = NULL;

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

	file = storage->bs_file_open(storage, file_name, BS_WRITEABLE);
	if (file == NULL) {
		HLOG_ERROR("Open ss_delmark.txt failed");
		g_message("Open ss_delmark.text failed");
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
	g_message("append size: %d", ret);
	
	storage->bs_file_close(storage, file);
	g_free(buf);
	HLOG_DEBUG("leave func %s successfully", __func__);
	return 0;
}

int ss2text(struct snapshot *ss, char *buf)
{
	HLOG_DEBUG("enter func %s", __func__);
	memset(buf, 0, sizeof(*buf));
	sprintf(buf, "%llu\n%s\n%s\n%llu\n%llu\n", ss->version, \
			ss->ss_name, ss->up_ss_name, ss->ime.inode_no, \
			ss->ime.inode_addr);
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

int dump_ss(struct back_storage *storage, struct snapshot *ss)
{
	HLOG_DEBUG("enter func %s", __func__);
	char buf[sizeof(struct snapshot) + 5];
	int ret = 0;
	ss2text(ss, buf);
#if 0
	g_message("%s", buf);
#endif
	if (0 > dump_ss_text(storage, buf)) {
		HLOG_ERROR("dump_ss_text()error");
		return -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
