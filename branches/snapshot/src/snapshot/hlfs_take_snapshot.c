/*
 *  src/snapshot/hlfs_take_snapshot.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 *  updated by Kelvin <kelvin.xupt@gmail.com>
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
    if(ctrl == NULL || ssname ==NULL){
		HLOG_ERROR("parameter error!");
        return -1;
    }
    int ret = 0;
	HLOG_DEBUG("The value of rw flag when taking snapshot:%d", ctrl->rw_inode_flag);

    g_mutex_lock(ctrl->hlfs_access_mutex);
    if(ctrl->rw_inode_flag == 0){
		HLOG_ERROR("error, snapshot can not take when readonly");
        g_mutex_unlock(ctrl->hlfs_access_mutex);
        return EHLFS_PERM;
    }
    g_mutex_unlock (ctrl->hlfs_access_mutex);
    
	if (HLFS_FS == (ret = is_first_start(ctrl->storage, SNAPSHOT_FILE, ALIVE_SNAPSHOT_FILE))) {
			goto out;	
	} else if (EHLFS_UNKNOWN == ret) {
		HLOG_ERROR("is first start error");
		return EHLFS_UNKNOWN;
	}
	struct snapshot *_ss = NULL;
	if (EHLFS_SSEXIST==(ret=load_snapshot_by_name(ctrl->storage,SNAPSHOT_FILE,&_ss,ssname))){
		HLOG_ERROR("snapshot %s is exist, use another snapshot name", ssname);
		return -1;
	}
out:;
	struct snapshot ss;
	memset(&ss, 0, sizeof(struct snapshot));
	ss.timestamp = get_current_time();
	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}
	g_strlcpy(ss.sname, ssname, strlen(ssname) + 1);
    g_mutex_lock(ctrl->hlfs_access_mutex);
	g_strlcpy(ss.up_sname,ctrl->alive_ss_name,strlen(ctrl->alive_ss_name) + 1);
	ss.inode_addr = ctrl->imap_entry.inode_addr;
	memset(ctrl->alive_ss_name, 0, (strlen(ctrl->alive_ss_name) + 1));
	g_strlcpy(ctrl->alive_ss_name,ss.sname, strlen(ss.sname) + 1);
    g_mutex_unlock (ctrl->hlfs_access_mutex);

    ret = dump_alive_snapshot(ctrl->storage,ALIVE_SNAPSHOT_FILE,&ss);
    if(ret!=0){
      HLOG_ERROR("dump snapshot alive error!");
    }
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,&ss);
    if(ret!=0){
      HLOG_ERROR("dump snapshot error!");
    }
	return ret;
}
