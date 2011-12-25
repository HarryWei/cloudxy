/*
 *  src/snapshot/hlfs_find_inode_by_name.c
 *
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#if 0
static int snapshot_delmark2text(const char *ssname, char *deltext) {
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	int n = sprintf(deltext, "%s\n", ssname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}
#endif

int 
hlfs_find_inode_by_name(const char *uri, 
						const char *sname, 
						uint64_t *inode_addr) {
    int ret = 0;
	g_message("enter func %s", __func__);
	return ret;
}
