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
#include "dentry_helper.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"

//extern uint64_t INODE_NO;

/*f_path: /xxx/dir/file
  1, Check if f_path exists.
  2, Check if /xxx/dir ecists.
 ? 3, Update /xxx/dir log to append file.
 ? 4, Append /xxx/dir/file log.
  5, Append file's dentry infos to dentry file.
 */
int hlfs_create(struct hlfs_ctrl *ctrl, const char *f_path, int is_dir) {
	g_message("9999 enter func %s", __func__);
    if(ctrl == NULL || f_path ==NULL){
		g_message("parameter error!");
        return -1;
    }
	if ('/' != f_path[0]) {
		g_message("please use absolute path.\n");
		return -1;
	}
	if (0 == g_strcmp0(f_path, "/")) {
		g_message("root dir has been created.");
		return -1;
	}
    int ret = 0;
	struct dentry *ds = NULL;
	gchar *dir_name = g_path_get_dirname(f_path);
	if (1 == load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, dir_name)) {
		g_message("%s not exist, please create first.", dir_name);
		ret = -1;
		goto out;
	}
	if (1 != load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, f_path)) {
		g_message("%s exist, replace a new file name.", f_path);
		ret = -1;
		goto out;
	}
	struct dentry _dentry;
	sprintf(_dentry.file_name, "%s", f_path);
	_dentry.inode_no = get_current_time();
	ret = dump_dentry(ctrl->storage, DENTRY_FILE, &_dentry);
	if (0 > ret) {
		g_message("dump dentry error.");
		goto out;
	}
	int size = 0;
	size = append_log(ctrl, NULL, 0, 0, 0, _dentry.inode_no, is_dir);
	if (0 > size) {
		g_message("Append log error!");
		ret = -1;
	}
out:
	g_free(dir_name);
	return ret;
}
