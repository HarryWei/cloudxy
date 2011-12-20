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

static int dump_snapshot_text(struct hlfs_ctrl *ctrl, 
		char *cptext, const char *cp_file)
{
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;

	if (-1 == ctrl->storage->bs_file_is_exist(ctrl->storage, cp_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = ctrl->storage->bs_file_create(ctrl->storage, cp_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s", cp_file);
			goto out;
		}
		ctrl->storage->bs_file_close(ctrl->storage, file);
	}
	file = ctrl->storage->bs_file_open(ctrl->storage, cp_file, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", cp_file);
		goto out;
	}
	len = strlen(cptext);
	HLOG_DEBUG("cp text is %s", cptext);
	if (0 > (ret = ctrl->storage->bs_file_append(ctrl->storage, file, cptext, len))) {
		HLOG_ERROR("write cp file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		ctrl->storage->bs_file_close(ctrl->storage, file);
	}
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}	

static int cp_2text(struct snapshot *cp, char *cp_text)
{
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	memset(cp_text, 0, sizeof(struct snapshot) * 2);
	int n = sprintf(cp_text, "%llu %llu %s\n", 
			cp->timestamp, cp->inode_addr, cp->sname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}

int hlfs_take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname)
{
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	struct inode *cur_inode = load_latest_inode(ctrl->storage);
	if (NULL == cur_inode) {
		HLOG_ERROR("get cur_inode error!");
		return -1;
	}
	struct snapshot *cp = NULL;
	cp = g_malloc0(sizeof(struct snapshot));
	if (NULL == cp) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	cp->timestamp = cur_inode->ctime;
	g_strlcpy(cp->sname, ssname, SNAME_LEN);
	cp->inode_addr = get_last_inode_storage_addr_in_seg(ctrl->storage, ctrl->last_segno);
	char cptext[sizeof(struct snapshot) * 2];
	uint32_t len = cp_2text(cp, cptext);
	int ret = dump_snapshot_text(ctrl, cptext, SNAPSHOT_USAGE_FILE);
	g_free(cp);
	g_free(cur_inode);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}
