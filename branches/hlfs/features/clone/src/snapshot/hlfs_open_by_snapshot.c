/*
  *  Copyright (C) 2012 Harry Wei <harryxiyou@gmail.com>
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

int hlfs_open_by_snapshot(struct hlfs_ctrl *ctrl,
					const char* snapshot,
					int flag) {
	//HLOG_DEBUG("enter func %s", __func__);
	if (NULL == ctrl || NULL == snapshot) {
		HLOG_ERROR("Parameter Error!");
		return -1;
	}
	if ((strlen(snapshot) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("snapshot name beyond max length");
		return -1;
	}
	int ret = 0;
	struct snapshot *ss = NULL;
	if (0 == ctrl->storage->bs_file_is_exist(ctrl->storage, SNAPSHOT_FILE)) {
		if (0 > (ret = load_snapshot_by_name(ctrl->storage, SNAPSHOT_FILE, &ss, snapshot))) {
			HLOG_ERROR("load ss by name error");
			g_free(ss);
			ret = -1;
			goto out;
		}
	} else {
		HLOG_ERROR("We have no snapshot at the moment");
		ret = -1;
		goto out;
	}
	struct inode *inode = load_inode(ctrl->storage,ss->inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load_inode error!");
		ret = -1;
		goto out;
	}

    memcpy(&(ctrl->inode), inode, sizeof(struct inode));
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
	memset(ctrl->alive_ss_name, 0, MAX_FILE_NAME_LEN);
	sprintf(ctrl->alive_ss_name, "%s", ss->sname);
    g_free(ss);
	ctrl->usage_ref += 1;
out:
	//HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
