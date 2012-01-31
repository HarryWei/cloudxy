/*
 *  src/snapshot/hlfs_open_by_inode.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 *  Updated by Kelvin <kelvin.xupt@gmail.com>
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
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	if(ctrl->usage_ref > 0){
		HLOG_DEBUG("This fs has opened by other,can not use it"); 
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
    ret = find_latest_alive_snapshot_before_time(ctrl->storage,ALIVE_SNAPSHOT_FILE, &_ss,inode->ctime);
    if(ret !=0){
       return -1; 
    }else{
	   sprintf(ss.up_sname,"%s",_ss->sname);
    }
    g_free(_ss);

    if(ctrl->alive_ss_name!=NULL){
       g_free(ctrl->alive_ss_name); 
    }
    ctrl->alive_ss_name = g_strdup(ss.sname);
    if(ctrl->rw_inode_flag == 1){
        ret = dump_alive_snapshot(ctrl->storage,ALIVE_SNAPSHOT_FILE,&ss);
        if(ret!=0){
            HLOG_ERROR("dump snapshot alive error!");
        }
        ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,&ss);
        if(ret!=0){
            HLOG_ERROR("dump snapshot error!");
        }
    }
out:
    if(inode!=NULL){
        g_free(inode);
    }
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

