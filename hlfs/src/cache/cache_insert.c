/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <string.h>
#include "cache.h"
#include "cache_helper.h"
#include "hlfs_log.h"

int cache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count, char *block_buf)
{
	//HLOG_DEBUG("--Entering func :%s", __func__);
	int ret = 0;
    int i =0;
    int idx=0;
    HLOG_DEBUG("--write to cache start bno:%d,block_count:%d--",start_block_no, block_count);
    for(i=start_block_no;i<start_block_no+block_count;i++){
        cache_insert_block(cache_ctrl,i,block_buf + idx*cache_ctrl->block_size);
        idx++;
    }
    return ret;
}


int cache_insert_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf){
    int ret = 0;
    block_t *block = cache_query(cache_ctrl,block_no);
    if(block!=NULL){
        HLOG_DEBUG("-- in cache,update dirty block --");
        memcpy(block->block,block_buf,cache_ctrl->block_size);  
    }else{
        uint32_t block_count =1;
        if (block_count + cache_ctrl->cache_size - get_cache_free_size(cache_ctrl) 
                >= (cache_ctrl->flush_trigger_level * cache_ctrl->cache_size) / 100) {
            HLOG_DEBUG("-- wake up flush worker for free more cache --");
            g_mutex_lock(cache_ctrl->cache_mutex);
            g_cond_signal(cache_ctrl->flush_waken_cond);
            g_mutex_unlock(cache_ctrl->cache_mutex);
            //printf("--free count:%d,block count:%d\n",get_cache_free_size(cache_ctrl),block_count);
            g_mutex_lock(cache_ctrl->cache_mutex);
            while (__get_cache_free_size(cache_ctrl) < block_count) {
                //HLOG_DEBUG("--wait continue--");
                g_cond_wait(cache_ctrl->writer_waken_cond, cache_ctrl->cache_mutex);
            }
            //HLOG_DEBUG("--continue to write--");
            g_mutex_unlock(cache_ctrl->cache_mutex);
        }
        write_cache(cache_ctrl,block_no,block_buf);
    }
    cache_ctrl->total_write_count++;
    return ret;
}

