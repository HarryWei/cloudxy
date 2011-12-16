/*
 *  src/snapshot/find_inode_before_time.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage.h"
#include "storage_helper.h"

static int get_inode_addr_by_time(struct back_storage *storage, 
									uint64_t timestamp,
									const char *segfile,
									uint64_t *inode_addr)
{
	bs_file_t file = storage->bs_file_open(storage, segfile, BS_READONLY);
	if (NULL == file) {
		HLOG_ERROR("file open error");
		return -1;
	}
	char *tmp_buf = (char *)g_malloc0(SEGMENT_SIZE);
	int count = storage->bs_file_pread(storage, file, tmp_buf, SEGMENT_SIZE, 0);
	if (0 > count) {
		HLOG_ERROR("read content error!");
		return -1;
	}
	int offset = 0;
	int tmp_time = 0;
	struct log_header *lh = NULL;
	struct inode_map_entry *imap = NULL;
	while (offset < count) {
		lh = (struct log_header *) (tmp_buf + offset);
		imap = (struct inode_map_entry *) (tmp_buf + lh->log_size - sizeof(struct inode_map_entry));
		if (timestamp > tmp_time && timestamp <= lh->ctime) {
			*inode_addr = imap->inode_addr;
			goto out;
		}
		tmp_time = lh->ctime;
		offset += lh->log_size;
	}
	HLOG_DEBUG("Can not find nearby timestamp in this segfile");
	return -1;
out:
	HLOG_DEBUG("Find nearby timestamp in this segfile");
	return 0;
}

int find_inode_before_time(const char *uri, uint64_t timestamp, uint64_t *inode_addr)
{
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		return -1;
	}
	int num_entries = 0;
	bs_file_info_t *infos = storage->bs_file_list_dir(storage, ".", &num_entries);
	if (NULL == infos) {
		HLOG_ERROR("get file list dir error!");
		return -1;
	}
	HLOG_DEBUG("there are %d files", num_entries);
	bs_file_info_t *info = infos;
	int i = 0;
	int tmp_time = 0;
	for (i = 0; i < num_entries; i++) {
		if (g_str_has_suffix(info->name, "seg")) {
			if (info->lmtime > timestamp && timestamp >= tmp_time) {
				int ret = get_inode_addr_by_time(storage, timestamp, info->name, inode_addr);
			}
			tmp_time = info->lmtime;
			info += 1;
		}
	}
	return ret;
}
