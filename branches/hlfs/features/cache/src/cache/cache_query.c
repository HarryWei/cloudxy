#include "cache.h"

int cache_query(CACHE_CTRL *cache_ctrl, uint64_t block_no, char **block)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	char *_block = NULL;
	
	if (cache_ctrl == NULL || block == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param error");
		return ret;
	}

	if (0 == g_hash_table_size(cache_ctrl->block_map)) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("The hash table of block_map is empty");
		return ret;
	}
	
	HLOG_DEBUG("block_no %llu will be queried", block_no);
	_block = (char *)g_hash_table_lookup(cache_ctrl->block_map, \
			(gpointer)&block_no);
	if (_block == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("NO item in hash table");
		return ret;
	}
	
	//*block = _block->block;
	memcpy(*block, _block, (size_t)cache_ctrl->block_size);
	
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}
