/*
  *  Copyright (C) 2012 Harry Wei <harryxiyou@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
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

static int 
get_iaddr_bytime_in_seg(struct back_storage *storage, 
						uint64_t timestamp,
						const char *segfile,
						uint64_t *inode_addr) {
    uint32_t segment_size = 0;
	uint32_t block_size = 0;
	uint64_t max_fs_size = 0;
	int ret = read_fs_meta(storage,&segment_size, &block_size,&max_fs_size);
	if (0 != ret) {
		HLOG_ERROR("read fs meta data error!");
		ret = -1;
		goto out;
	}
	uint32_t _SEGMENT_SIZE = segment_size;
	bs_file_t file = storage->bs_file_open(storage, segfile, BS_READONLY);
	if (NULL == file) {
		HLOG_ERROR("file open error");
		ret = -1;
		goto out;
	}
	//HLOG_DEBUG("SEGMENT SIZE is %d", _SEGMENT_SIZE);
	char *tmp_buf = (char *)g_malloc0(_SEGMENT_SIZE);
	if (NULL == tmp_buf) {
		HLOG_ERROR("%s -- allocate error", __func__);
		ret = -1;
		goto out;
	}
	int count = storage->bs_file_pread(storage, file, tmp_buf,_SEGMENT_SIZE, 0);
	if (0 > count) {
		HLOG_ERROR("read content error!");
		ret = -1;
		goto out;
	}
	//HLOG_DEBUG("count is %d", count);
	int offset = 0;
	int first_inode_flag = 0;
//	uint64_t tmp_time = 0;
//	uint64_t tmp_inode_addr = 0;
	struct log_header *lh = NULL;
	struct inode_map_entry *imap = NULL;
	struct inode *inode = NULL;
#if 0
	if (0 == timestamp) {
		HLOG_ERROR("We can not find the inode addr before time 0");
		ret = -1;
		goto out;
	}
#endif
	while (offset < count) {
		lh = (struct log_header *) (tmp_buf + offset);
		//g_message("%s -- log size is %d", __func__, lh->log_size);
		imap = (struct inode_map_entry *) (tmp_buf + offset + lh->log_size - sizeof(struct inode_map_entry));
		//g_message("%s -- This inode addr is %llu", __func__, imap->inode_addr);
		inode = (struct inode *) (tmp_buf + offset + lh->log_size - sizeof(struct inode_map_entry) - sizeof(struct inode));
		//g_message("%s -- This inode's mtime is %llu", __func__, inode->mtime);
	    //*inode_addr = imap->inode_addr;
		if ((0 == first_inode_flag) && (timestamp < inode->mtime)) {
			HLOG_ERROR("We can not find the inode addr before the timestamp");
			ret = -1;
			goto out;
		}
		if (timestamp == inode->mtime) {
			*inode_addr = imap->inode_addr;
			goto out;
		}	
		if (timestamp < inode->mtime) {
			struct inode_map_entry *pre_imap = (struct inode_map_entry *) (tmp_buf + offset - sizeof(struct inode_map_entry));
			*inode_addr = pre_imap->inode_addr;
			goto out;
		}	
//		tmp_inode_addr = imap->inode_addr;
//		tmp_time = inode->mtime;
		first_inode_flag = 1;
		offset += lh->log_size;
	}
	if (timestamp > inode->mtime) {
		*inode_addr = imap->inode_addr;
		goto out;
	}
	HLOG_ERROR("We can not find the inode addr before the timestamp");
	ret = -1;
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	g_free(tmp_buf);
	//g_message("leave func %s", __func__);
	return ret;
}

static int 
sort_finfo_with_time(gconstpointer linfo, gconstpointer rinfo) {
	uint64_t llmtime = ((bs_file_info_t *) linfo)->lmtime;
	uint64_t rlmtime = ((bs_file_info_t *) rinfo)->lmtime;
	if (llmtime > rlmtime) {
		return 1;
	} else if (llmtime == rlmtime) {
		return 0;
	} else {
		return -1;
	}
}

int 
hlfs_find_inode_before_time(const char *uri, 
							uint64_t timestamp,
							uint64_t *inode_addr) {
    if (uri == NULL) {
        return -1;
    }
    int ret = 0;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("init storage handler error!");
        return -1;
    }
    int num_entries = 0;
    bs_file_info_t *infos = storage->bs_file_list_dir(storage, ".", &num_entries);
    if (NULL == infos){ 
        HLOG_ERROR("get file list dir error!");
        return -1;
    }
    HLOG_DEBUG("there are %d files", num_entries);
    bs_file_info_t *info = infos;
    int i = 0;
    GList *info_list = NULL;
    for (i = 0; i < num_entries; i++) {
        if (g_str_has_suffix(info->name, "seg")) {
            info_list = g_list_append(info_list,info);
		}
		info += 1;
	}
	if (0 == g_list_length(info_list)) {
		HLOG_ERROR("There is no seg file in back storage ???");
		ret = 1;
		goto out;
	}
	info_list = g_list_sort(info_list,sort_finfo_with_time);
	//HLOG_DEBUG("list length is %d", g_list_length(info_list));
    for(i = 0; i < g_list_length(info_list); i++){
        info = g_list_nth_data(info_list,i);
        if(info->lmtime >= timestamp){
           break; 
        }
    }
#if 0
    if(i == 0) {//timestamp < any seg's time
       ret = -1;
       goto out;
    }
#endif
    /* to find inode in give seg */
	char segfile[512];
	memset(segfile, 0, 512);
    uint32_t segno = get_segfile_no(info->name);
    build_segfile_name(segno,segfile);
    ret = get_iaddr_bytime_in_seg(storage, timestamp, segfile, inode_addr);
out:
	g_list_free(info_list);
	return ret;
}
