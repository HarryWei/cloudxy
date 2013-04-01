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

static void
print_hlfs_dirs_files(gpointer key, gpointer value, gpointer user_value) {
	char *dentry = (char *) key;
	g_print("%s\n", dentry);
	return ;
}

int hlfs_list(struct hlfs_ctrl *ctrl) {
	g_message("9999 enter func %s", __func__);
    if(ctrl == NULL){
		g_message("parameter error!");
        return -1;
    }
    int ret = 0;
#if 0
	if ('/' != f_path[0]) {
		g_message("please use absolute path.\n");
		return -1;
	}
	struct dentry *ds = NULL;
	gchar *dir_name = g_path_get_dirname(f_path);
	if (1 == load_dentry_by_name(ctrl->storage, DENTRY_FILE, &ds, dir_name)) {
		g_message("%s not exist, please create first.", dir_name);
		ret = -1;
		goto out;
	}
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
#endif
	GHashTable *ds_hashtable = g_hash_table_new(g_str_hash, g_str_equal);
	ret = load_all_dentry(ctrl->storage, DENTRY_FILE, ds_hashtable);
	if (0 > ret) {
		g_message("load all dentry error.");
		goto out;
	}
	g_hash_table_foreach(ds_hashtable, print_hlfs_dirs_files, NULL);
out:
	g_hash_table_destroy(ds_hashtable);
	return ret;
}
