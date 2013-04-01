/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *  
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"

int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname) 
{
	//HLOG_DEBUG("enter func %s", __func__);
	//HLOG_DEBUG("create ssname is %s", ssname);
    if(ctrl == NULL || ssname ==NULL){
		HLOG_ERROR("parameter error!");
        return -1;
    }
    int ret = 0;
	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}

    g_mutex_lock(ctrl->hlfs_access_mutex);
    if(ctrl->rw_inode_flag == 0){
		HLOG_ERROR("error, snapshot can not take when readonly");
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        return -1;
    }
    g_mutex_unlock (ctrl->hlfs_access_mutex);
    
	struct snapshot *_ss = NULL;
	if (0 == (ret=load_snapshot_by_name(ctrl->storage,SNAPSHOT_FILE,&_ss,ssname))){
		HLOG_ERROR("snapshot %s is exist, use another snapshot name", ssname);
		return -1;
	}else{
		HLOG_DEBUG("snapshot %s is not exist , create it ", ssname);
    }

	struct snapshot ss;
	memset(&ss, 0, sizeof(struct snapshot));
	ss.timestamp = get_current_time();
	g_strlcpy(ss.sname, ssname, strlen(ssname) + 1);
       g_mutex_lock(ctrl->hlfs_access_mutex);
	sprintf(ss.up_sname, "%s", ctrl->alive_ss_name);
	ss.inode_addr = ctrl->imap_entry.inode_addr;
	memset(ctrl->alive_ss_name, 0, MAX_FILE_NAME_LEN);
	sprintf(ctrl->alive_ss_name, "%s", ss.sname);
       g_mutex_unlock (ctrl->hlfs_access_mutex);

    ret = dump_alive_snapshot(ctrl->storage,ALIVE_SNAPSHOT_FILE,&ss);
    if(ret!=0){
      HLOG_ERROR("dump snapshot alive error!");
      return -1;
    }
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,&ss);
    if(ret!=0){
      HLOG_ERROR("dump snapshot error!");
      return -1;
    }
	HLOG_INFO("Take Snapshot Succ- snapshot_name:%s,last_segno:%d,last_offset:%d",
    						      ssname,
    						      ctrl->last_segno,
    						      ctrl->last_offset);
	return ret;
}
