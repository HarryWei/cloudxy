/*
  *  Copyright (C) 2013 Harry Wei <harryxiyou@gmail.com>
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
#include "dentry.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"

/*
   1, check if f_path exists.
   2, get f_path's dentry structure.
   3, init ctrl.
 */

int hlfs_fclose(struct hlfs_ctrl *ctrl, const char *f_path) {
	g_message("9999 enter func %s", __func__);
    if(ctrl == NULL || f_path ==NULL){
		HLOG_ERROR("parameter error!");
        return -1;
    }
    int ret = 0;
	struct dentry *ds = NULL;
	ret = load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, f_path);
	if (1 == ret) {
		g_message("f_path not exist.");
		goto out;
	}
#if 0
	g_message("ds->file_name: %s, ds->inode_no: %llu", ds->file_name, ds->inode_no);
	uint64_t inode_addr = 0;
	ret = get_latest_inode_addr_by_inodeno(ctrl->storage, ds->inode_no, &inode_addr);
	if (0 > ret) {
		g_message("get latest inode addr by inode no. error.");
		goto out;
	}
	char *offset = (char *) load_field(ctrl->storage, inode_addr);
	if (NULL == offset) {
		g_message("load field error.");
		goto out;
	}
	struct inode *i = (struct inode *) offset;
	struct inode_map_entry *im = (struct inode_map_entry *) (offset + sizeof(struct inode));
	memcpy(&(ctrl->inode), i, sizeof(struct inode));
	memcpy(&(ctrl->imap_entry), im, sizeof(struct inode_map_entry));
	ctrl->rw_inode_flag = flag;
	g_free(offset);
#endif
	ret = hlfs_close(ctrl);
	if (0 > ret) {
		g_message("close file %s error.", f_path);
		goto out;
	}
out:
	g_message("9999 leave func %s", __func__);
	return ret;
}
