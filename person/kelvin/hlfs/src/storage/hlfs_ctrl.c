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

int hlfs_clean_stop(struct hlfs_ctrl *ctrl)
{
	//HLOG_DEBUG("enter func %s", __func__);
   	gint *is_start_clean  = &ctrl->ctrl_region->is_start_clean;
   	g_atomic_int_set(is_start_clean,0);
	//HLOG_DEBUG("leave func %s", __func__);
   	return 0;
}

int hlfs_clean_start(struct hlfs_ctrl *ctrl)
{
	//HLOG_DEBUG("enter func %s", __func__);
   	gint *is_start_clean  = &ctrl->ctrl_region->is_start_clean;
   	g_atomic_int_set(is_start_clean,1);
	//HLOG_DEBUG("leave func %s", __func__);
   	return 0;
}

int hlfs_set_clean_level(struct hlfs_ctrl *ctrl,unsigned int alive_bytes)
{
	//HLOG_DEBUG("enter func %s", __func__);
   	gint *clean_copy_waterlevel = &ctrl->ctrl_region->copy_waterlevel;
   	g_atomic_int_set(clean_copy_waterlevel,alive_bytes);
	//HLOG_DEBUG("leave func %s", __func__);
   	return 0;
}

int hlfs_set_user_ctrl_region(struct hlfs_ctrl *ctrl,CTRL_REGION_T* ctrl_region)
{
	//HLOG_DEBUG("enter func %s", __func__);
    g_atomic_pointer_set(&ctrl->ctrl_region,ctrl_region);
	//HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

int hlfs_flush(struct hlfs_ctrl *ctrl)
{
    if(ctrl->cctrl!=NULL){
       cache_sync(ctrl->cctrl); 
    }
    return 0;
}

