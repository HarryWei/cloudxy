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
#include "storage.h"
#include "storage_helper.h"
#include "cache.h"

int deinit_hlfs(struct hlfs_ctrl * ctrl)
{
	//HLOG_DEBUG("enter func:%s",__func__);
	if (NULL == ctrl) {
		HLOG_ERROR("ctrl is null");
		return -1;
    }
    if(0!=deinit_storage_handler(ctrl->storage)){
		HLOG_ERROR("deinit_hlfs error");
       	return -1; 
    }
    if(ctrl->cctrl!=NULL){
       //HLOG_DEBUG("-destroy cache-");
       cache_destroy(ctrl->cctrl);
    }
    if(ctrl->icache!=NULL){
       //HLOG_DEBUG("-destroy icache-");
       icache_destroy(ctrl->icache);
    }
    if(ctrl->family != NULL){
	  family_destroy(ctrl->family);
    }		
    ctrl->ctrl_region->is_start_clean = 0;
    g_thread_join(ctrl->seg_clean_thread);
    //g_async_queue_unref(ctrl->write_req_aqueue);
    //g_async_queue_unref(ctrl->write_rsp_aqueue);
    g_mutex_free(ctrl->hlfs_access_mutex);
    /* fix it :we do not free ctrl_region now */
    g_free(ctrl->storage);
    g_free(ctrl);
    //HLOG_DEBUG("leave func:%s",__func__);
    return 0;
} 
