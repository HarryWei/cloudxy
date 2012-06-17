/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "glib.h"
#include "icache.h"

ICACHE_CTRL *icache_new(){
    //HLOG_DEBUG("--Entering func %s", __func__);
	struct icache_ctrl *icache_ctrl = NULL;
	if (NULL == (icache_ctrl = (struct icache_ctrl *)g_malloc0(sizeof(struct \
						icache_ctrl)))) {
		HLOG_ERROR("--Error:Apply for mem");
		return NULL;
	}
    return icache_ctrl;
}
int icache_init(ICACHE_CTRL *icache_ctrl,
		uint32_t iblock_size,
		uint32_t icache_size,
		uint32_t invalidate_trigger_level,
		uint32_t invalidate_once_size){
    //HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	
	if (NULL == icache_ctrl) {
		HLOG_ERROR("--Error:Param error");
		return -EHLFS_PARAM;
	}

	HLOG_DEBUG("--iblock_size:%llu,icache_size:%llu,\
		invalidate_trigger_level:%llu,invalidate_once_size:%llu,%s", iblock_size, \
		icache_size,invalidate_trigger_level,invalidate_once_size,__func__);
    icache_ctrl->iblock_size = iblock_size;
    icache_ctrl->icache_size = icache_size;
    icache_ctrl->invalidate_trigger_level = invalidate_trigger_level;
    icache_ctrl->invalidate_once_size = invalidate_once_size;
#if 0
	if (NULL == (cache_ctrl->block_cache = (GTrashStack *)g_malloc0(sizeof \
					(GTrashStack)))) {
		HLOG_ERROR("--Error:Apply for cache");
		ret = -EHLFS_MEM;
		goto err;
	}
#endif
	int i;
    for (i = 0; i < icache_size; i++) {
        iblock_t *_iblock = g_malloc0(sizeof(iblock_t));
        g_assert(_iblock != NULL);
        _iblock->iblock = (char *)g_malloc0(iblock_size);
        g_assert(_iblock->iblock != NULL);
        g_trash_stack_push(&icache_ctrl->iblock_cache,_iblock);
    }
    //HLOG_DEBUG("--cache container init over!--");
	
	if (NULL == (icache_ctrl->iblock_lru = 	g_queue_new())) {
		HLOG_ERROR("--Error:Apply for LRU queue");
		ret = -EHLFS_MEM;
		goto err;
    }	
    //HLOG_DEBUG("--lru block queue init over!--");
	if (NULL == (icache_ctrl->iblock_map = g_hash_table_new(g_int_hash,g_int_equal))) {
		HLOG_ERROR("--Error:Apply for block_map");
		ret = -EHLFS_MEM;
		goto err;
    }
    //HLOG_DEBUG("--iblock_map init over!--");
    //g_thread_init(NULL);
    //if (NULL == (icache_ctrl->icache_mutex = g_mutex_new())) {
    //    HLOG_ERROR("--Error:Apply for mutext");
    //    ret = -EHLFS_MEM;
    //    goto err;
    //}
	//HLOG_DEBUG("--Leaving func %s", __func__);
    return ret;
err:
    if (icache_ctrl->icache_mutex)
        g_mutex_free(icache_ctrl->icache_mutex);
    if (icache_ctrl->iblock_map)
        g_hash_table_destroy(icache_ctrl->iblock_map);
    if (icache_ctrl->iblock_lru)
        g_queue_free(icache_ctrl->iblock_lru);
    if (icache_ctrl->iblock_cache) {
        while (g_trash_stack_height(&icache_ctrl->iblock_cache) != 0) {
           iblock_t *_iblock = (iblock_t *)g_trash_stack_pop(&icache_ctrl->iblock_cache);
           if (_iblock->iblock != NULL)
               g_free(_iblock->iblock);
           g_free(_iblock);
        }
    }
	g_free(icache_ctrl);
	//HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}


