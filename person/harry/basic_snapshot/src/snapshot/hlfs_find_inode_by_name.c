/*
 * This file implements the snapshot module API hlfs_find_inode_by_name()
 * 
 * By Kelvin <kelvin.xupt@gmail.com>
 *
 * Time : 2011.12.26
 */
#include <glib.h>
#include <stdint.h>
#include "storage.h"
#include "storage_helper.h"
#include "snapshot.h"
#include "hlfs_log.h"

int hlfs_find_inode_by_name(const char *uri, const char *sname, uint64_t *inode_addr) {
	int ret = 0;
	struct snapshot *ss = NULL;
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		HLOG_ERROR("init storage handler error!");
		ret = -1;
		goto out;
	}
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	if (NULL == ss) {
		HLOG_ERROR("Allocate error!");
		ret = -1;
		goto out;
	}
	if (0 > (ret = load_ss_by_name(storage, ss, sname))) {
		HLOG_ERROR("load ss by name error");
		g_free(ss);
		ret = -1;
		goto out;
	} else if (1 == ret) {
		HLOG_ERROR("We can not find the snapshot name");
		goto out;
	}
	*inode_addr = ss->inode_addr;
out:
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
