/*
 *  src/snapshot/hlfs_open_by_inode.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
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
int 
hlfs_open_by_inode(struct hlfs_ctrl *ctrl,
					uint64_t inode_addr,
					int flag) {
	int ret = 0;
	struct inode *inode = load_inode(ctrl->storage, inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("%s -- load_inode error!", __func__);
		ret = -1;
		goto out;
	}
	memcpy(&ctrl->inode, inode, sizeof(struct inode));
	struct inode_map_entry imap = {
		.inode_no = HLFS_INODE_NO,
		.inode_addr = inode_addr,
	};
	memcpy(&ctrl->imap_entry, &imap, sizeof(struct inode_map_entry));
	if (0 == flag) {	//the common condition
		ctrl->rw_inode_flag = 0;
	} else if (1 == flag) {	//forbid hlfs_write
		ctrl->rw_inode_flag = 1;
	} else {
		HLOG_ERROR("%s -- the bad flag for hlfs open by inode", __func__);
		ret = -1;
	}
out:
	g_free(inode);
	return ret;
}
