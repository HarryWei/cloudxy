/*
 *  src/snapshot/hlfs_take_snapshot.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 *  updated by Kelvin <kelvin.xupt@gmail.com>
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
    if(ctrl == NULL || ssname ==NULL){
        return -1;
    }
    
	int ret = 0;
	struct snapshot *cp = g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	struct inode *inode = load_inode(ctrl->storage, ctrl->imap_entry.inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load inode error");
		g_free(cp);
		return -1;
	}
	cp->timestamp = inode->ctime;
	g_free(inode);

	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}
	
	g_strlcpy(cp->sname, ssname, strlen(ssname) + 1);
	g_strlcpy(cp->up_sname,ctrl->alive_ss_name,strlen(ctrl->alive_ss_name) + 1);
	cp->inode_addr = ctrl->imap_entry.inode_addr;
	sprintf(cp->up_sname, "%s", ctrl->alive_ss_name);
	sprintf(ctrl->alive_ss_name, "%s", cp->sname);

    g_mutex_lock (ctrl->hlfs_access_mutex);
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,cp);
    g_mutex_unlock (ctrl->hlfs_access_mutex);

	g_free(cp);
	return ret;
}
