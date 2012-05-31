/*
  *  Copyright (C) 2012      Harry Wei <harryxiyou@gmail.com>
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
#include "comm_define.h"
#include "hlfs_log.h"

int hlfs_open_by_inode(struct hlfs_ctrl *ctrl,
					uint64_t inode_addr,
					int flag) {
	//HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	if(ctrl->usage_ref > 0){
		HLOG_DEBUG("This fs has opened by other,can not use it"); 
        return -1;
	}
	if(get_segno(inode_addr) < ctrl->start_segno){
		 HLOG_ERROR("sorry ,not support open inode in parent hlfs now!");
		 return -1;
	}	
	struct inode *inode = load_inode(ctrl->storage, inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load_inode error!");
		ret = -1;
		goto out;
	}

    memcpy(&ctrl->inode,inode,sizeof(struct inode));
    ctrl->imap_entry.inode_no = HLFS_INODE_NO;
    ctrl->imap_entry.inode_addr = inode_addr;

	if (0 == flag) {
		ctrl->rw_inode_flag = 0;
	} else if (1 == flag) {
		ctrl->rw_inode_flag = 1;
	} else {
		HLOG_ERROR("the bad flag for hlfs open by inode");
		ret = -1;
        goto out;
	}
    
    struct snapshot ss;
   	sprintf(ss.sname, "%llu",inode_addr);
	ss.inode_addr = inode_addr;
	ss.timestamp = get_current_time();

    struct snapshot *_ss=NULL;
	if (0 == ctrl->storage->bs_file_is_exist(ctrl->storage, SNAPSHOT_FILE)) {
    	ret = find_latest_alive_snapshot_before_time(ctrl->storage, 
													SNAPSHOT_FILE, 
													ALIVE_SNAPSHOT_FILE, 
													&_ss,inode->mtime);
    	if(ret !=0){
       		ret = -1;
			goto out;
    	}else{
	   		sprintf(ss.up_sname,"%s",_ss->sname);
    	}
    	g_free(_ss);
	} else {
		//HLOG_DEBUG("do not need read alive snapshot file");
		memset(ss.up_sname, 0, MAX_FILE_NAME_LEN);
	}
	memset(ctrl->alive_ss_name, 0, MAX_FILE_NAME_LEN);
	sprintf(ctrl->alive_ss_name, "%s", ss.sname);
    if(ctrl->rw_inode_flag == 1){
        ret = dump_alive_snapshot(ctrl->storage,ALIVE_SNAPSHOT_FILE,&ss);
        if(ret!=0){
            HLOG_ERROR("dump snapshot alive error!");
			ret = -1;
			goto out;
        }
        ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,&ss);
        if(ret!=0){
            HLOG_ERROR("dump snapshot error!");
			ret = -1;
			goto out;
        }
    }
	ctrl->usage_ref += 1;
out:
    if(inode!=NULL){
        g_free(inode);
    }
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

