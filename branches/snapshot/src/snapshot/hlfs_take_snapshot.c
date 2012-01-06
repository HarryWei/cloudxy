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

int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname) {
    int ret = 0;
    if(ctrl == NULL || ssname ==NULL){
        return -1;
    }
	struct snapshot *cp = g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	cp->timestamp = get_current_time();
	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}
	g_strlcpy(cp->sname,ssname,strlen(ssname) + 1);
	g_strlcpy(cp->up_sname,ctrl->alive_ss_name,strlen(ctrl->alive_ss_name) + 1);
	cp->inode_addr = ctrl->imap_entry.inode_addr;
	memset(ctrl->alive_ss_name, 0, strlen(ctrl->alive_ss_name));
	sprintf(ctrl->alive_ss_name, "%s", ssname);
    g_mutex_lock (ctrl->hlfs_access_mutex);
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,cp);
    g_mutex_unlock (ctrl->hlfs_access_mutex);
	g_free(cp);
	return ret;
}
