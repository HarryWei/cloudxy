#include "cache.h"
#include "cache_helper.h"

int cache_query_block(CACHE_CTRL *cache_ctrl, uint64_t block_no, char *block_buf)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	block_t *block = NULL;
	if (cache_ctrl == NULL || block_buf == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param error");
		return ret;
	}

	if (0 == cache_ctrl->cache_size - get_cache_free_size(cache_ctrl)) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("The hash table of block_map is empty");
		return ret;
	}
	
	HLOG_DEBUG("block_no %llu will be queried",block_no);
    g_mutex_lock(cache_ctrl->cache_mutex);
	block = (block_t*)g_hash_table_lookup(cache_ctrl->block_map, \
			(gpointer)&block_no);
    g_mutex_unlock(cache_ctrl->cache_mutex);
	if (block == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("NO item in hash table");
		return ret;
	}
	g_assert(block_no == block->block_no);
	memcpy(block_buf, block->block, (size_t)cache_ctrl->block_size);
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}
