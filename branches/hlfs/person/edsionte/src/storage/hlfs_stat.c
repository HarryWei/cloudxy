/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "storage_helper.h"

int hlfs_lstat(const char*uri,HLFS_STAT_T *stat){
	//HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    if(NULL == uri|| NULL == stat){
	   HLOG_ERROR("param error");
          return -1; 
    }
    struct back_storage *storage = init_storage_handler(uri);
    if(storage == NULL){
       ret = -1;
       goto out;
    }
    uint32_t seg_size,block_size,last_segno,last_offset;
    uint64_t max_fs_size;
    ret = read_fs_meta(storage,&seg_size,&block_size,&max_fs_size);
    if(ret != 0){
         HLOG_ERROR("can not read fs meta data");
         goto out;
    }
    ret =get_cur_latest_segment_info(storage,&last_segno,&last_offset);
    if(ret !=0){
         HLOG_ERROR("can not read fs meta data");
         goto out;
    }
    stat->seg_size    = seg_size;
    stat->block_size  = block_size;
    stat->max_fs_size = max_fs_size;
    g_strlcpy(stat->fsname,g_basename(uri),MAX_FILE_NAME_LEN);
    stat->last_segno  = last_segno;
    stat->last_offset = last_offset;
out:
    if(storage!=NULL){
       deinit_storage_handler(storage);
    }
	//HLOG_DEBUG("leave func %s", __func__);
    return ret;   
}

int hlfs_stat(struct hlfs_ctrl* ctrl,HLFS_STAT_T *stat){
	//HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    if(NULL == ctrl || NULL == stat){
	   HLOG_ERROR("param error");
       return -1; 
    }
    stat->seg_size    = ctrl->sb.seg_size;
    stat->block_size  = ctrl->sb.block_size;
    stat->max_fs_size = ctrl->sb.max_fs_size;
    g_strlcpy(stat->fsname,ctrl->sb.fsname,MAX_FILE_NAME_LEN);
    stat->last_segno  = ctrl->last_segno;
    stat->last_offset  = ctrl->last_offset;
	//HLOG_DEBUG("leave func %s", __func__);
    return ret;   
}
