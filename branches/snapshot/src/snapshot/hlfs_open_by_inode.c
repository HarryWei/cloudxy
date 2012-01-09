/*
 *  src/snapshot/hlfs_open_by_inode.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 *  Updated by Kelvin <kelvin.xupt@gmail.com>
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "comm_define.h"
#include "hlfs_log.h"

#if 0
int creat_auto_snapshot(struct hlfs_ctrl *ctrl, uint64_t inode_addr)
{
	HLOG_DEBUG("enter func %s", __func__);
	char *up_ss_name = NULL;
	struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot)); 
	sprintf(ss->sname, "%llu", inode_addr);
	find_up_ss_name_of_inode(ctrl, inode_addr, &up_ss_name);
	sprintf(ss->up_sname, "%s", up_ss_name);
	ss->inode_addr = inode_addr;

	struct inode* inode = load_inode(ctrl->storage, inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load inode error");
		g_free(ss);
		g_free(up_ss_name);
		return -1;
	}
	ss->timestamp = inode->ctime;
	dump_snapshot(ctrl->storage, SNAPSHOT_FILE, ss);
	g_free(ss);
	g_free(up_ss_name);
	g_free(inode);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}
#endif

int hlfs_open_by_inode(struct hlfs_ctrl *ctrl,
					uint64_t inode_addr,
					int flag) {
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	struct inode *inode = load_inode(ctrl->storage, inode_addr);
	if (inode == NULL) {
		HLOG_ERROR("load_inode error!");
		ret = -1;
		goto out;
	}
	memcpy(&ctrl->inode, inode, sizeof(struct inode));
	struct inode_map_entry imap = {
		.inode_no = HLFS_INODE_NO,
		.inode_addr = inode_addr,
	};
	memcpy(&ctrl->imap_entry, &imap, sizeof(struct inode_map_entry));
	if (0 == flag) {	//the common condition
		ctrl->rw_inode_flag = 0;
	} else if (1 == flag) {	//forbid hlfs_write
		ctrl->rw_inode_flag = 1;
	} else {
		HLOG_ERROR("the bad flag for hlfs open by inode");
		ret = -1;
	}
	ctrl->alive_ss_name = NULL;
	if (0 > find_up_ss_name_of_inode(ctrl, inode_addr, &ctrl->alive_ss_name)) {
		HLOG_ERROR("find up ss name error!");
		ret = -1;
	}
#if 0
	ctrl->alive_ss_name = (char *)g_malloc0(MAX_FILE_NAME_LEN);
	GHashTable *ss_hashtable = g_hash_table_new_full(g_direct_hash, g_direct_equal, \
			NULL, NULL);
	if (load_all_ss_use_inode_addr_keys(ctrl->storage, ss_hashtable) < 0) {
		HLOG_ERROR("error - load_all_ss");
		g_free(inode);
		g_hash_table_destroy(ss_hashtable);
		return -1;
	}

	struct snapshot *ss = NULL;
	ss = g_hash_table_lookup(ss_hashtable, GINT_TO_POINTER(inode_addr));
	if (ss != NULL) {
		sprintf(ctrl->alive_ss_name, "%s", ss->sname);
		HLOG_DEBUG("alive_ss_name:%s", ss->sname);
	} else {
		creat_auto_snapshot(ctrl, inode_addr);
	}
	g_hash_table_destroy(ss_hashtable);
#endif
out:
	g_free(inode);
	return ret;
	HLOG_DEBUG("leave func %s", __func__);
}
