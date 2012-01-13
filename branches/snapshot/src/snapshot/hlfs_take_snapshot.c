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
/*check start*/
	HLOG_DEBUG("enter func : %s", __func__);
	int ret = 0;
    if (ctrl == NULL || ssname == NULL || ctrl->alive_ss_name == NULL) {
		HLOG_ERROR("parameter error");
        return -1;
    }
	if ((strlen(ssname) + 1) > HLFS_FILE_NAME_MAX) {
		HLOG_ERROR("error, snapshot name beyond max length!");
		return -1;
	}
/*Check if ssname is existing in the snapshot.txt*/
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, \
			NULL, NULL);
	int res = load_all_ss(ctrl->storage, ss_hashtable);
	if (res == -1) {
		HLOG_DEBUG("no snapshot exists");
	}
	struct snapshot *_ss = NULL;
	_ss = g_hash_table_lookup(ss_hashtable, ssname);
	if (_ss != NULL) {
		g_hash_table_destroy(ss_hashtable);
		HLOG_ERROR("ssname is existing!");
		return -2;
	}
	g_hash_table_destroy(ss_hashtable);
/*check end*/

	struct snapshot *cp = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -3;
	}
	
	cp->timestamp = ctrl->inode.ctime;
	g_strlcpy(cp->sname, ssname, strlen(ssname) + 1);
	g_strlcpy(cp->up_sname, ctrl->alive_ss_name, strlen(ctrl->alive_ss_name) + 1);
	cp->inode_addr = ctrl->imap_entry.inode_addr;

	memset(ctrl->alive_ss_name, 0, (strlen(ctrl->alive_ss_name) + 1));
	sprintf(ctrl->alive_ss_name, "%s", cp->sname);

	ret = dump_snapshot(ctrl->storage, SNAPSHOT_FILE, cp);
	if (ret < 0) {
		HLOG_ERROR("dump_snapshot error");
		g_free(cp);
		HLOG_DEBUG("leave func : %s", __func__);
		return -3;
	}

	g_free(cp);
	HLOG_DEBUG("leave func : %s", __func__);
	return ret;
}
