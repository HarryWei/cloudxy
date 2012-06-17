/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "cache.h"

extern int flush_work(gpointer data);

CACHE_CTRL *cache_new()
{
	//HLOG_DEBUG("--Entering func %s", __func__);
	struct cache_ctrl *cache_ctrl = NULL;
	if (NULL == (cache_ctrl = (struct cache_ctrl *)g_malloc0(sizeof(struct \
						cache_ctrl)))) {
		HLOG_ERROR("--Error:Apply for mem");
		return NULL;
	}
	//HLOG_DEBUG("--Leaving func %s", __func__);
    return cache_ctrl;
}

int cache_init(CACHE_CTRL *cache_ctrl, 
		uint32_t block_size, \
		uint32_t cache_size, \
		uint32_t flush_interval, \
		uint32_t flush_trigger_level, \
		uint32_t flush_once_size)
{
	//HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	
	if (NULL == cache_ctrl) {
		HLOG_ERROR("--Error:Param error");
		return -EHLFS_PARAM;
	}

	//HLOG_DEBUG("--block_size:%llu,cache_size:%llu,flush_interval:%llu, \
	//		flush_trigger_level:%llu,flush_once_size:%llu,%s", block_size, \
	//		cache_size,flush_interval,flush_trigger_level,flush_once_size,__func__);

    cache_ctrl->block_size = block_size;
    cache_ctrl->cache_size = cache_size;
    cache_ctrl->flush_interval = flush_interval;
    cache_ctrl->flush_trigger_level = flush_trigger_level;
    cache_ctrl->flush_once_size = flush_once_size;
#if 0
	if (NULL == (cache_ctrl->block_cache = (GTrashStack *)g_malloc0(sizeof \
					(GTrashStack)))) {
		HLOG_ERROR("--Error:Apply for cache");
		ret = -EHLFS_MEM;
		goto err;
	}
#endif
	int i;
    for (i = 0; i < cache_size; i++) {
        block_t *_block = g_malloc0(sizeof(block_t));
        g_assert(_block != NULL);
        _block->block = (char *)g_malloc0(block_size);
        g_assert(_block->block != NULL);
        g_trash_stack_push(&cache_ctrl->block_cache,_block);
    }
    //HLOG_DEBUG("--cache container init over!--");
	
	if (NULL == (cache_ctrl->dirty_block = 	g_queue_new())) {
		HLOG_ERROR("--Error:Apply for LRU queue");
		ret = -EHLFS_MEM;
		goto err;
    }	
    //HLOG_DEBUG("--dirty block queue init over!--");
	if (NULL == (cache_ctrl->block_map = g_hash_table_new(g_int_hash,g_int_equal))) {
		HLOG_ERROR("--Error:Apply for block_map");
		ret = -EHLFS_MEM;
		goto err;
    }
    //HLOG_DEBUG("--dirty block_map init over!--");
    if(g_thread_get_initialized ()){
		g_thread_init(NULL);
	    if (NULL == (cache_ctrl->cache_mutex = g_mutex_new())) {
	        HLOG_ERROR("--Error:Apply for mutext");
	        ret = -EHLFS_MEM;
	        goto err;
	    }
    }

    cache_ctrl->flush_waken_cond =  g_cond_new();
    cache_ctrl->writer_waken_cond = g_cond_new();
    cache_ctrl->flush_worker_should_exit = 0;
    cache_ctrl->flush_worker = g_thread_create((GThreadFunc)flush_work, cache_ctrl, TRUE, NULL);
    g_assert(cache_ctrl->flush_worker);
    //HLOG_DEBUG("--flush worker init over!--");
	//HLOG_DEBUG("--Leaving func %s", __func__);
    return ret;
err:
    if (cache_ctrl->cache_mutex)
        g_mutex_free(cache_ctrl->cache_mutex);
    if (cache_ctrl->flush_waken_cond)
        g_cond_free(cache_ctrl->flush_waken_cond);
    if (cache_ctrl->writer_waken_cond)
        g_cond_free(cache_ctrl->writer_waken_cond);
    if (cache_ctrl->block_map)
        g_hash_table_destroy(cache_ctrl->block_map);
    if (cache_ctrl->dirty_block)
        g_queue_free(cache_ctrl->dirty_block);
    if (cache_ctrl->block_cache) {
        while (g_trash_stack_height(&cache_ctrl->block_cache) != 0) {
           block_t *_block = (block_t *)g_trash_stack_pop(&cache_ctrl->block_cache);
           if (_block->block != NULL)
               g_free(_block->block);
           g_free(_block);
        }
    }
	g_free(cache_ctrl);
	//HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}
