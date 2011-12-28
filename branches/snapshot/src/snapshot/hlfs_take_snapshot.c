/*
 *  src/snapshot/hlfs_take_snapshot.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "misc.h"

int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname)
{
    int ret = 0;
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
    if(ctrl == NULL || ssname ==NULL){
        return -1;
    }
	struct snapshot *cp = g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	cp->timestamp = get_current_time();
	g_strlcpy(cp->sname,ssname,HLFS_FILE_NAME_MAX);
	cp->inode_addr = ctrl->imap_entry.inode_addr;
    g_mutex_lock (ctrl->hlfs_access_mutex);
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,cp);
    g_mutex_unlock (ctrl->hlfs_access_mutex);
	g_free(cp);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}
