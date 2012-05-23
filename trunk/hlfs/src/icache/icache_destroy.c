/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "glib.h"
#include "icache.h"

int icache_destroy(ICACHE_CTRL *icache_ctrl){
	//
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0, i = 0;
	if (icache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param still NULL");
		return ret;
	}
   
	
	/*destroy the Hash table*/
    if (icache_ctrl->iblock_map){
        g_hash_table_remove_all(icache_ctrl->iblock_map);
        g_hash_table_destroy(icache_ctrl->iblock_map);
    }
	
	/*destroy the LRU list*/
    if (icache_ctrl->iblock_lru) {
        while(!g_queue_is_empty(icache_ctrl->iblock_lru)){
            iblock_t *iblock = (gpointer)g_queue_pop_head(icache_ctrl->iblock_lru);
            g_free(iblock->iblock);
            g_free(iblock);
        }
        g_queue_free(icache_ctrl->iblock_lru);
    }
	
	/*destroy the cache container*/
    if (icache_ctrl->iblock_cache) {
		int i = 0;
        while (g_trash_stack_height(&icache_ctrl->iblock_cache) != 0) {
			HLOG_DEBUG("g_trash_stack_height: %d", g_trash_stack_height(&icache_ctrl->iblock_cache));
			iblock_t *_iblock = (iblock_t *)g_trash_stack_pop(&icache_ctrl->iblock_cache);
			if (_iblock->iblock != NULL) 
				g_free(_iblock->iblock);
			g_free(_iblock);
			i++;
			//HLOG_DEBUG("----destroy %d succ", i);
        }
    }
    g_mutex_free (icache_ctrl->icache_mutex);
  
	g_free(icache_ctrl);
	//HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}


