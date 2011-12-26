/*
 * This file implements the snapshot module API hlfs_find_inode_by_name()
 * 
 * By Kelvin <kelvin.xupt@gmail.com>
 *
 * Time : 2011.12.26
 */

#include "snapshot.h"

int hlfs_find_inode_by_name(const char *uri, const char *sname, \
		uint64_t *inode_addr)
{
	HLOG_DEBUG("enter func %s", __func__);
	struct back_storage *storage = init_storage_handler(uri);
	struct snapshot *ss;
	int ret = 0;
	
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));

	if (0 > (ret = load_ss_by_name(storage, ss, sname))) {
		HLOG_ERROR("load ss by name error");
		return ret;
	}
	
	*inode_addr = ss->inode_addr;
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
