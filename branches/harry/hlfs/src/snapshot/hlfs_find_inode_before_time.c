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

static int get_inode_addr_in_seg(struct back_storage *storage, 
									uint64_t timestamp,
									const char *segfile,
									uint64_t *inode_addr) {
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

static GList *get_last_inode_info_in_segs(struct back_storage *storage, 
										bs_file_info_t *infos,
										struct seg_info *seg_infos,
										int num_entries) {
	int ret = 0;
	int i = 0;
	bs_file_t file = NULL;
	bs_file_info_t *info = infos;
	struct seg_info *_seg_info = seg_infos;
	gchar **v = NULL;
	char *endptr = NULL;
	char *tmp_buf = NULL;
	struct inode inode;
	GList *list = NULL;

#if 0
	tmp_buf = g_malloc0(sizeof(char) * sizeof(struct inode));
	if (NULL == tmp_buf) {
		g_message("%s -- allocate error!", __func__);
		return -1;
	}
#endif
	memset(&inode, 0, sizeof(struct inode));
	for (i = 0; i < num_entries; i++) {
		if (g_str_has_suffix(info->name, "seg")) {
			v = g_strsplit(info->name, ".", 2);
			_seg_info->segno = strtoul(v[0], &endptr, 0);
			file = storage->bs_file_open(storage, info->name, BS_READONLY);
			if (NULL == file) {
				g_message("%s -- open file error!", __func__);
				ret = -1;
				goto out;
			}
			uint32_t size = storage->bs_file_pread(storage, file, &inode, sizeof(struct inode),
							info->size - sizeof(struct inode) - sizeof(struct inode_map_entry));
			if (sizeof(struct inode) != size) {
				g_message("%s -- read inode error!", __func__);
				ret = -1;
				goto out;
			}
			_seg_info->lmtime = inode.mtime;
			list = g_list_append(list, _seg_info);
			_seg_info += 1;
		}
		info += 1;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	g_strfreev(v);
	return list;
}

static sort_segs_with_time(struct seg_info *seg_infos, int count)  {
	struct 
}

int find_inode_before_time(const char *uri, uint64_t timestamp, uint64_t *inode_addr) {
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
	int count = 0;
	for (i = 0; i < num_entries; i++) {
		if (g_str_has_suffix(info->name, "seg")) {
#if 0
			if (info->lmtime > timestamp && timestamp >= tmp_time) {
				int ret = get_inode_addr_by_time(storage, timestamp, info->name, inode_addr);
			}
			tmp_time = info->lmtime;
			info += 1;
#endif
			count += 1;
		}
		info += 1;
	}
	struct seg_info *seg_infos = (struct seg_info *)g_malloc0(sizeof(struct seg_info) * count);
	if (NULL == seg_infos) {
		g_message("allocate seg_info error!");
		return -1;
	}
	GList *list = NULL;
	list = get_last_inode_info_in_segs(storage, infos, seg_infos, num_entries);
	sort_segs_with_time(seg_infos, count);
	int segno = 0;
	find_seg_with_timestamp(seg_infos, timestamp, &segno);
	char segfile[128];
	sprintf(segfile, "%d%s%s", segno, ".", "seg");
	get_inode_addr_in_seg(storage, timestamp, segfile, inode_addr);
	return ret;
}
