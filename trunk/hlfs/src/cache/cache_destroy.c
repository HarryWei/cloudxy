/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "cache.h"

int cache_destroy(CACHE_CTRL *cache_ctrl)
{
	//HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0, i = 0;
	if (cache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param still NULL");
		return ret;
	}
    
	if (cache_ctrl->flush_worker_should_exit != 1) {
	    cache_ctrl->flush_worker_should_exit = 1;
        g_cond_signal(cache_ctrl->flush_waken_cond);
 	    g_thread_join(cache_ctrl->flush_worker);
    }
	
	/*destroy the Hash table*/
    if (cache_ctrl->block_map){
        g_hash_table_remove_all(cache_ctrl->block_map);
        g_hash_table_destroy(cache_ctrl->block_map);
    }
	
	/*destroy the LRU list*/
    if (cache_ctrl->dirty_block) {
        while(!g_queue_is_empty(cache_ctrl->dirty_block)){
            block_t *block = (gpointer)g_queue_pop_head(cache_ctrl->dirty_block);
            g_free(block->block);
            g_free(block);
        }
        g_queue_free(cache_ctrl->dirty_block);
    }
	
	/*destroy the cache container*/
    if (cache_ctrl->block_cache) {
		int i = 0;
        while (g_trash_stack_height(&cache_ctrl->block_cache) != 0) {
			//HLOG_DEBUG("g_trash_stack_height: %d", g_trash_stack_height(&cache_ctrl->block_cache));
			block_t *_block = (block_t *)g_trash_stack_pop(&cache_ctrl->block_cache);
			if (_block->block != NULL) 
				g_free(_block->block);
			g_free(_block);
			i++;
			//HLOG_DEBUG("----destroy %d succ", i);
        }
    }
    g_mutex_free (cache_ctrl->cache_mutex);
    //g_mutex_free(cache_ctrl->cache_mutex);  
    //g_cond_clear(cache_ctrl->flush_waken_cond);
    g_cond_free(cache_ctrl->flush_waken_cond);     
    //g_cond_clear(cache_ctrl->writer_waken_cond);
    g_cond_free(cache_ctrl->writer_waken_cond);     
    /* do not free write callback user param */ 
	g_free(cache_ctrl);
	//HLOG_INFO("-- Data Block Cache Destroy Over --");
	return ret;
}
