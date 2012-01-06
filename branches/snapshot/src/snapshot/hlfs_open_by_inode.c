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

int hlfs_open_by_inode(struct hlfs_ctrl *ctrl,
					uint64_t inode_addr,
					int flag) {
	int ret = 0;
	struct inode *inode = load_inode(ctrl->storage, inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load_inode error!");
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
		HLOG_ERROR("the bad flag for hlfs open by inode");
		ret = -1;
	}
	if (0 > find_up_ss_name_of_inode(ctrl, inode_addr, &ctrl->alive_ss_name)) {
		HLOG_ERROR("find up ss name error!");
		ret = -1;
	}
#if 0
	ctrl->alive_ss_name = (char *)g_malloc0(MAX_FILE_NAME_LEN);
	if (NULL == ctrl->alive_ss_name) {
		HLOG_ERROR("Allocate error!");
		return -1;
	}
#endif
out:
	g_free(inode);
	return ret;
}
