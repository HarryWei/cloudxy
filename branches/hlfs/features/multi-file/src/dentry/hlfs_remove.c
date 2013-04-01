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
  2, Append file's dentry infos to dentry file.
  ?3, Remove relevant log datas in seg files.
 */
int hlfs_remove(struct hlfs_ctrl *ctrl, const char *f_path) {
	g_message("9999 enter func %s", __func__);
    if(ctrl == NULL || f_path ==NULL){
		g_message("parameter error!");
        return -1;
    }
	if ('/' != f_path[0]) {
		g_message("please use absolute path.\n");
		return -1;
	}
    int ret = 0;
	struct dentry *ds = NULL;
#if 0
	gchar *dir_name = g_path_get_dirname(f_path);
	if (1 == load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, dir_name)) {
		g_message("%s not exist, please create first.", dir_name);
		ret = -1;
		goto out;
	}
#endif
	if (1 == load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, f_path)) {
		g_message("%s not exist, please create first.", f_path);
		ret = -1;
		goto out;
	}
	ret = dump_dentry_delmark(ctrl->storage, DENTRY_FILE, f_path);
	if (0 > ret) {
		g_message("dump dentry error.");
		goto out;
	}
out:
	return ret;
}
