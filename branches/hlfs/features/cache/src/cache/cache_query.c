#include "cache.h"

int cache_query(CACHE_CTRL *cache_ctrl, uint64_t block_no, char **block)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	
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

	*block = (char *)g_hash_table_lookup(cache_ctrl->block_map, (gconstpointer)block_no);

	if (*block == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("NO item in hash table");
		return ret;
	}

	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}
