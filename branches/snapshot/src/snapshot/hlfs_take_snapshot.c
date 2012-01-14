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
#include "comm_define.h"

int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname) 
{
    if(ctrl == NULL || ssname ==NULL){
		HLOG_ERROR("parameter error!");
        return -1;
    }
	/* record the up snapshot name of a snapshot */
	if (NULL == ctrl->alive_ss_name) {
		ctrl->alive_ss_name = (char *)g_malloc0(MAX_FILE_NAME_LEN);
		if (NULL == ctrl->alive_ss_name) {
			HLOG_ERROR("allocate error!");
			return EHLFS_MEM;
		}
		create_auto_snapshot(ctrl, ctrl->imap_entry.inode_addr);
		snprintf(ctrl->alive_ss_name, MAX_FILE_NAME_LEN, "%llu", ctrl->imap_entry.inode_addr);
	}
    if (0 == is_sname_exist(ctrl->storage, ssname)) {
		HLOG_ERROR("This snaoshot name has been used, please repick another one!");
		return -2;
	}
	int ret = 0;
	struct snapshot *cp = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	cp->timestamp = get_current_time();
	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}
	g_strlcpy(cp->sname, ssname, strlen(ssname) + 1);
	g_strlcpy(cp->up_sname,ctrl->alive_ss_name,strlen(ctrl->alive_ss_name) + 1);
	cp->inode_addr = ctrl->imap_entry.inode_addr;
	memset(ctrl->alive_ss_name, 0, (strlen(ctrl->alive_ss_name) + 1));
	sprintf(ctrl->alive_ss_name, "%s", cp->sname);

    g_mutex_lock (ctrl->hlfs_access_mutex);
	ret = dump_snapshot(ctrl->storage,SNAPSHOT_FILE,cp);
    g_mutex_unlock (ctrl->hlfs_access_mutex);

	g_free(cp);
	return ret;
}
