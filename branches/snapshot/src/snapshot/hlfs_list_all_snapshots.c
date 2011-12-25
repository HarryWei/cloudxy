/*
 *  src/snapshot/hlfs_list_all_snapshots.c
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
hlfs_list_all_snapshots(const char *uri, 
						char **ssname) {
    int ret = 0;
	g_message("enter func %s", __func__);
	return ret;
}
