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
					uint64_t *ctime,
					uint64_t *length) {
	int ret = 0;
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == uri) {
		g_message("%s -- init storage handler error!", __func__);
		ret = -1;
		goto out;
	}
	struct inode *inode = load_inode(storage, inode_addr);
	if (NULL == inode) {
		g_message("%s -- load inode error!", __func__);
		ret = -1;
		goto out;
	}
	*ctime = inode->ctime;
	*length = inode->length;
out:
	g_free(storage);
	g_free(inode);
	return ret;
}
