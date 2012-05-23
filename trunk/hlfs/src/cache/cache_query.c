/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "cache.h"
#include "cache_helper.h"

int cache_query_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    block_t * block = cache_query(cache_ctrl,block_no);
	if (block == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_DEBUG("NO item in hash table");
		return ret;
	}
	HLOG_DEBUG("--read block no:%d",block->block_no);
	g_assert(block_no == block->block_no);
	memcpy(block_buf, block->block, (size_t)cache_ctrl->block_size);
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;

}
gboolean  cache_block_exist(CACHE_CTRL *cache_ctrl, uint32_t block_no){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    block_t * block = cache_query(cache_ctrl,block_no);
    if(block==NULL){
       return FALSE;   
    }
    return TRUE;
}
