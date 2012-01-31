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

int hlfs_open_by_snapshot(struct hlfs_ctrl *ctrl,
					const char* snapshot,
					int flag) {
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	struct snapshot *ss;
	if (0 > (ret = load_snapshot_by_name(ctrl->storage, &ss, snapshot))) {
		HLOG_ERROR("load ss by name error");
		g_free(ss);
		ret = -1;
		goto out;
	} else if (1 == ret) {
		HLOG_ERROR("We can not find the snapshot name");
		goto out;
	}
	struct inode *inode = load_inode(ctrl->storage,ss->inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load_inode error!");
		ret = -1;
		goto out;
	}

    strncpy((char *) (&(ctrl->inode)), (const char *) inode, sizeof(struct inode));
	g_free(inode);
    ctrl->imap_entry.inode_no = HLFS_INODE_NO;
    ctrl->imap_entry.inode_addr = ss->inode_addr;

	if (0 == flag) {
		ctrl->rw_inode_flag = 0;
	} else if (1 == flag) {
		ctrl->rw_inode_flag = 1;
	} else {
		HLOG_ERROR("the bad flag for hlfs open by inode");
		ret = -1;
	}
    if(ctrl->alive_ss_name!=NULL){
       g_free(ctrl->alive_ss_name); 
    }
    ctrl->alive_ss_name = g_strdup(ss->sname);
    g_free(ss);
out:
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
