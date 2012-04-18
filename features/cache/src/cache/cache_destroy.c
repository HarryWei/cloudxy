#include "cache.h"

int cache_destroy(CACHE_CTRL *cache_ctrl)
{
	HLOG_DEBUG("--Entering func %s", __func__);
   	
	int ret = 0, i = 0;
	if (cache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param still NULL");
		return ret;
	}
 	
	cache_ctrl->flush_worker_should_exit = 1;
 	g_thread_join(cache_ctrl->flush_worker);
	if (cache_ctrl->dirty_block != NULL) {
		//TODO
	}

	/*destroy the LRU list*/
    if (cache_ctrl->dirty_block)
        g_queue_free(cache_ctrl->dirty_block);
	
	/*destroy the Hash table*/
    if (cache_ctrl->block_map)
        g_hash_table_destroy(cache_ctrl->block_map);
	
	/*destroy the cache container*/
    if (cache_ctrl->block_cache) {
        while (g_trash_stack_height(&cache_ctrl->block_cache) != 0) {
           block_t *_block = (block_t *)g_trash_stack_pop(&cache_ctrl->block_cache);
           if (_block->block != NULL)
               g_free(_block->block);
           g_free(_block);
        }
    }
	
	g_free(cache_ctrl);

	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}
