#include "cache.h"
#include "hlfs_log.h"
#include "comm_define.h"

CACHE_CTRL *cache_new()
{
	HLOG_DEBUG("--Entering func %s", __func__);

	struct cache_ctrl *cache_ctrl = NULL;
	if (NULL == (cache_ctrl = (struct cache_ctrl *)g_malloc0(sizeof(struct cache_ctrl)))) {
		HLOG_ERROR("--Error:Apply for mem");
		return NULL;
	}

	return cache_ctrl;

	HLOG_DEBUG("--Leaving func %s", __func__);
}

int cache_init(CACHE_CTRL *cache_ctrl, \
		uint64_t block_size, \
		uint64_t cache_size, \
		uint64_t flush_interval, \
		uint64_t flush_trigger_level, \
		uint64_t flush_once_size)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	
	if (NULL == cache_ctrl) {
		HLOG_ERROR("--Error:Param error");
		return -EHLFS_PARAM;
	}

	if (NULL == (cache_ctrl->cache_mutex = g_mutext_new())) {
		HLOG_ERROR("--Error:Apply for mutext");
		return -1;
	}

	if (NULL == (cache_ctrl->block_cache = (GTrashStack *)g_malloc0(sizeof(GTrashStack)))) {
		HLOG_ERROR("--Error:Apply for cache");
		ret = -EHLFS_MEM;
		goto err;
	}

	if (NULL == (cache_ctrl->dirty_block = 	g_queue_new())) {
		HLOG_ERROR("--Error:Apply for LRU queue");
		ret = -1;
		goto err1;
	}

	if (NULL == (cache_ctrl->block_map = g_hash_new_full(g_direct_hash, \
					g_direct_equal, NULL, NULL))) {
		HLOG_ERROR("--Error:Apply for block_map");
		ret = -1;
		goto err2;
	}//TODO	

	return ret;
err2:
	g_queue_free(cache_ctrl->dirty_block);
err1:
	g_free(cache_ctrl->block_cache);
err:
	g_mutex_free(cache_ctrl->cache_mutex);
	
	return ret;
	HLOG_DEBUG("--Leaving func %s", __func__);
}
