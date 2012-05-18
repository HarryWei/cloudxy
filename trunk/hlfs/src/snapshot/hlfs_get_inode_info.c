/*
 *  src/snapshot/hlfs_get_inode_info.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"

int
hlfs_get_inode_info(const char *uri,
					uint64_t inode_addr,
					uint64_t *mtime,
					uint64_t *length) {
	int ret = 0;
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == uri) {
		HLOG_ERROR("init storage handler error!");
		ret = -1;
		goto out;
	}
	struct inode *inode = load_inode(storage, inode_addr);
	if (NULL == inode) {
		HLOG_ERROR("load inode error!");
		ret = -1;
		goto out;
	}
	*mtime = inode->mtime;
	*length = inode->length;
out:
	g_free(storage);
	g_free(inode);
	return ret;
}
