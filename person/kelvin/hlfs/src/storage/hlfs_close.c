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
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "logger.h"
#include "cache.h"
/*
 * hlfs_close: close a file. s
 * @param ctrl: The global control structure.
 * return: 0 is returned on success, else -1 is returned.
 */
int hlfs_close(struct hlfs_ctrl *ctrl){
    //HLOG_DEBUG("enter func:%s",__func__);
    if (NULL == ctrl) {
	    HLOG_ERROR("Params Error");
	    return -1;
    }
    int ret =0;
    if(ctrl->last_wsegfile_handler!=NULL){
          ret = ctrl->storage->bs_file_close(ctrl->storage,(bs_file_t)ctrl->last_wsegfile_handler);
	   ctrl->last_wsegfile_handler = NULL;
    }
    if(ctrl->last_rsegfile_handler!=NULL){
          ret = ctrl->storage->bs_file_close(ctrl->storage,(bs_file_t)ctrl->last_rsegfile_handler);
	   ctrl->last_rsegfile_handler = NULL;
    }
    ctrl->usage_ref--;
    if(ctrl->cctrl!=NULL){
       //HLOG_DEBUG("before close hlfs,sync all dirty block");
          cache_sync(ctrl->cctrl); 
    }
    HLOG_INFO("hlfs close over !");
    //HLOG_DEBUG("leave func:%s",__func__);
    return ret;
}        

