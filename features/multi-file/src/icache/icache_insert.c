/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "glib.h"
#include "icache.h"



//int icache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int icache_insert_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf){
    //HLOG_DEBUG("--enter fun %s", __func__);
    icache_ctrl->total_write_count++;
    iblock_t *_iblock = icache_query(icache_ctrl,iblock_no);
    if(_iblock!= NULL){
        HLOG_DEBUG("--update exist iblock--");
        memcpy(_iblock->iblock,iblock_buf,icache_ctrl->iblock_size);  
	    return 0;
    }
    g_mutex_lock(icache_ctrl->icache_mutex);
    if((icache_ctrl->icache_size - g_queue_get_length(icache_ctrl->iblock_lru)) < icache_ctrl->invalidate_once_size){
       HLOG_DEBUG("-- invalidate iblock for free iblock --");
       int count = icache_ctrl->invalidate_once_size;
	   while(count --){
	    iblock_t * iblock = g_queue_pop_tail(icache_ctrl->iblock_lru);
           if(iblock == NULL){
               break;
           }
           HLOG_DEBUG("--pop iblock :%d--",iblock->iblock_no);
		   g_hash_table_remove(icache_ctrl->iblock_map,&(iblock->iblock_no));
	       g_trash_stack_push(&icache_ctrl->iblock_cache,iblock);
	   }
       HLOG_DEBUG("now has %d iblock",g_queue_get_length(icache_ctrl->iblock_lru));
    }
    iblock_t *iblock = g_trash_stack_pop(&icache_ctrl->iblock_cache);
    HLOG_DEBUG("--insert iblock_no:%d--",iblock_no);
    iblock->iblock_no = iblock_no;
    memcpy(iblock->iblock,iblock_buf, icache_ctrl->iblock_size);
    g_hash_table_insert(icache_ctrl->iblock_map,&(iblock->iblock_no),iblock);
    g_queue_push_head(icache_ctrl->iblock_lru,iblock);
    g_mutex_unlock(icache_ctrl->icache_mutex);
    return 0;
}


