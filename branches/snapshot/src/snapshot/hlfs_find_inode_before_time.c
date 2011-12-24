/*
 *  src/snapshot/hlfs_find_inode_before_time.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "snapshot.h"
#include "storage.h"
#include "storage_helper.h"
//#include "comm_define.h"
#if 0
static int 
get_inode_addr_in_seg(struct back_storage *storage, 
						uint64_t timestamp,
						const char *segfile,
						uint64_t *inode_addr) {
	g_message("enter func %s", __func__);
	uint64_t SEGMENT_SIZE = 64 * 1000 * 1000;
	int ret = 0;
	bs_file_t file = storage->bs_file_open(storage, segfile, BS_READONLY);
	if (NULL == file) {
		g_message("file open error");
		ret = -1;
		goto out;
	}
	g_message("SEGMENT SIZE is %llu", SEGMENT_SIZE);
	char *tmp_buf = (char *)g_malloc0(SEGMENT_SIZE * sizeof(char));
	if (NULL == tmp_buf) {
		g_message("%s -- allocate error", __func__);
		ret = -1;
		goto out;
	}
	int count = storage->bs_file_pread(storage, file, tmp_buf, SEGMENT_SIZE, 0);
	if (0 > count) {
		g_message("read content error!");
		ret = -1;
		goto out;
	}
	g_message("%s -- count is %d", __func__, count);
	int offset = 0;
	uint64_t tmp_time = 0;
	struct log_header *lh = NULL;
	struct inode_map_entry *imap = NULL;
	struct inode *inode = NULL;
	if (0 == timestamp) {
		lh = (struct log_header *) (tmp_buf + offset);
		imap = (struct inode_map_entry *) (tmp_buf + lh->log_size - sizeof(struct inode_map_entry));
		*inode_addr = imap->inode_addr;
		goto out;
	}
	while (offset < count) {
		lh = (struct log_header *) (tmp_buf + offset);
		g_message("%s -- log size is %d", __func__, lh->log_size);
		imap = (struct inode_map_entry *) (tmp_buf + lh->log_size - sizeof(struct inode_map_entry));
		g_message("%s -- This inode addr is %llu", __func__, imap->inode_addr);
		inode = (struct inode *) (tmp_buf + lh->log_size - sizeof(struct inode_map_entry) - sizeof(struct inode));
		g_message("%s -- tmp_time is %llu, This inode's mtime is %llu", __func__, tmp_time, inode->mtime);
		if ((timestamp > tmp_time) && (timestamp <= inode->mtime)) {
			*inode_addr = imap->inode_addr;
			goto out;
		}
		tmp_time = inode->mtime;
		offset += lh->log_size;
	}
	*inode_addr = imap->inode_addr;		// timestamp > the last inode's mtime
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	return ret;
}

static int 
get_last_inode_info_in_segs(struct back_storage *storage, 
							bs_file_info_t *infos,
							struct seg_info *seg_infos,
							int num_entries, 
							GList **list) {
	int ret = 0;
	int i = 0;
	bs_file_t file = NULL;
	bs_file_info_t *info = infos;
	struct seg_info *_seg_info = seg_infos;
	gchar **v = NULL;
	char *endptr = NULL;
	char *tmp_buf = NULL;
	struct inode inode;
//	GList *list = NULL;

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
			uint32_t size = storage->bs_file_pread(storage, file, (const char *) &inode, sizeof(struct inode),
							info->size - sizeof(struct inode) - sizeof(struct inode_map_entry));
			if (sizeof(struct inode) != size) {
				g_message("%s -- read inode error!", __func__);
				ret = -1;
				goto out;
			}
			_seg_info->lmtime = inode.mtime;
			*list = g_list_append(*list, _seg_info);
			_seg_info += 1;
		}
		info += 1;
	}
	*list = g_list_first(*list);
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	g_strfreev(v);
	return ret;
}

static int 
sort_segs_with_time(gconstpointer seg_info1, 
					gconstpointer seg_info2) {
	uint64_t lmtime1 = 0;
	uint64_t lmtime2 = 0;

	lmtime1 = ((struct seg_info *) seg_info1)->lmtime;
	lmtime2 = ((struct seg_info *) seg_info2)->lmtime;
	if (lmtime1 > lmtime2) {
		return 1;
	} else if (lmtime1 == lmtime2) {
		return 0;
	} else {
		return -1;
	}
}

static int
find_seg_with_timestamp(GList *list,
						uint64_t timestamp,
						int *segno) {
	int ret = 0;
	int i = 0;
	uint64_t tmp_time = 0;
	GList *_list = g_list_copy(list);
	g_message("%s -- list length is %d", __func__, g_list_length(list));
	g_message("%s -- timestamp is %llu", __func__, timestamp);
	GList *__list = NULL;
	__list = g_list_first(list);
	if (timestamp <= (((struct seg_info *)(__list->data))->lmtime)) {
			*segno = ((struct seg_info *)(__list->data))->segno;
			goto out;
	}
	__list = g_list_last(list);
	if (timestamp >= (((struct seg_info *)(__list->data))->lmtime)) {
			*segno = ((struct seg_info *)(__list->data))->segno;
			goto out;
	}
	while (i < g_list_length(list)) {
		uint64_t lmtime = ((struct seg_info *)(_list->data))->lmtime;
		g_message("%s -- lmtime is %llu", __func__, lmtime);
		if ((timestamp > tmp_time) && (timestamp <= lmtime)) {
			*segno = ((struct seg_info *)(_list->data))->segno;
			goto out;
		}
#if 0
		if (timestamp == 0) {
			*segno = ((struct seg_info *)(_list->data))->segno;
			goto out;
		}
#endif
		tmp_time = lmtime;
		_list = _list->next;
		i += 1;
	}
	g_message("We can not find inode about your timestamp, any matter???");
	ret = -1;
out:
	g_list_free(_list);
	return ret;
}

int 
hlfs_find_inode_before_time(const char *uri, 
							uint64_t timestamp, 
							uint64_t *inode_addr) {
	int ret = 0;
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		g_message("init storage handler error!");
		return -1;
	}
	int num_entries = 0;
	bs_file_info_t *infos = storage->bs_file_list_dir(storage, ".", &num_entries);
	if (NULL == infos) {
		g_message("get file list dir error!");
		return -1;
	}
	g_message("there are %d files", num_entries);
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
	g_message("count is %d", count);
	struct seg_info *seg_infos = (struct seg_info *)g_malloc0(sizeof(struct seg_info) * count);
	if (NULL == seg_infos) {
		g_message("allocate seg_info error!");
		return -1;
	}
	GList *list = NULL;
	if (-1 == get_last_inode_info_in_segs(storage, infos, seg_infos, num_entries, &list)) {
		g_message("%s -- get_last_inode_info_in_segs error!", __func__);
		ret = -1;
		goto out;
	}
	g_message("list length is %d", g_list_length(list));
	int tmp = 0;
	for (tmp = 0; tmp < g_list_length(list); tmp++) {
		uint64_t lmtime = ((struct seg_info *)(list->data))->lmtime;
		uint32_t segno = ((struct seg_info *)(list->data))->segno;
		g_message("This is %d item, segno is %d, lmtime is %llu", tmp, segno, lmtime);
	}
	//sort_segs_with_time(seg_infos, count, list);
	list = g_list_sort(list, sort_segs_with_time);
	g_message("After sort, seg infos >>");
	for (tmp = 0; tmp < g_list_length(list); tmp++) {
		uint64_t lmtime = ((struct seg_info *)(list->data))->lmtime;
		uint32_t segno = ((struct seg_info *)(list->data))->segno;
		g_message("This is %d item, segno is %d, lmtime is %llu", tmp, segno, lmtime);
	}
	int segno = 0;
	if (-1 == find_seg_with_timestamp(list, timestamp, &segno)) {
		g_message("%s -- find_seg_with_timestamp error!", __func__);
		ret = -1;
		goto out;
	}
	g_message("find seg no is %d", segno);
	char segfile[128];
	sprintf(segfile, "%d%s%s", segno, ".", "seg");
	g_message("segfile is %s", segfile);
	if (-1 == get_inode_addr_in_seg(storage, timestamp, segfile, inode_addr)) {
		g_message("%s -- get_inode_addr_in_seg error!", __func__);
		ret = -1;
		goto out;
	}
out:
	g_list_free(list);
	return ret;
}
#endif
