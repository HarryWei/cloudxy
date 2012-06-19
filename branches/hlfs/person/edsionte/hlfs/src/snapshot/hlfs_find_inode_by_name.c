/*
  *  Copyright (C) 2012 By Kelvin <kelvin.xupt@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <glib.h>
#include <stdint.h>
#include "storage.h"
#include "storage_helper.h"
#include "snapshot.h"
#include "hlfs_log.h"

int hlfs_find_inode_by_name(const char *uri, const char *sname, uint64_t *inode_addr) {
	int ret = 0;
	struct snapshot *ss = NULL;
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		ret = -1;
		goto out;
	}
	if (0 > (ret = load_snapshot_by_name(storage, SNAPSHOT_FILE, &ss, sname))) {
		HLOG_ERROR("load ss by name error");
		g_free(ss);
		ret = -1;
		goto out;
	}
	*inode_addr = ss->inode_addr;
out:
	//HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
