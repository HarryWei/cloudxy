#include "snapshot.h"
#include "hlfs_log.h"

int dump_ss_delmark(struct back_storage *storage, const char *ss_name)
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
		g_message("ss_delmark.txt is not exist...\ncreat it\n");
		file = storage->bs_file_create(storage, file_name);
		if (file == NULL) {
			HLOG_ERROR("Create ss_delmark.txt failed");
			g_free(buf);
			return -1;
		}
		storage->bs_file_close(storage, file);
	}

	file == storage->bs_file_open(storage, file_name, BS_WRITEABLE);
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
	return 0;
}
