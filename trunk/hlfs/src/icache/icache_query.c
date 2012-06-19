/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "glib.h"
#include "icache.h"

iblock_t * icache_query(ICACHE_CTRL *icache_ctrl,uint32_t iblock_no){
	//HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	if (icache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param error");
		return NULL;
	}
	//HLOG_DEBUG("iblock_no %llu will be queried",iblock_no);
       g_mutex_lock(icache_ctrl->icache_mutex);
	iblock_t * iblock = (iblock_t*)g_hash_table_lookup(icache_ctrl->iblock_map,&(iblock_no));
       g_mutex_unlock(icache_ctrl->icache_mutex);
       //if(_iblock!=NULL){
       //iblock_buf = (char*)g_malloc0(icache_ctrl->iblock_size);
       //g_assert(iblock_no == _iblock->iblock_no);
       //memcpy(iblock_buf,_iblock->iblock,icache_ctrl->iblock_size);
       //}
    return iblock;
}


int icache_query_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf){
	//HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	icache_ctrl->total_read_count++;
	g_mutex_lock(icache_ctrl->icache_mutex);
       iblock_t * iblock  = (iblock_t*)g_hash_table_lookup(icache_ctrl->iblock_map,&(iblock_no));
       if (iblock == NULL) {
           ret = -EHLFS_NOITEM;
           HLOG_ERROR("NO item in hash table");
	       g_mutex_unlock(icache_ctrl->icache_mutex);
           return ret;
      }
      g_queue_remove(icache_ctrl->iblock_lru,iblock);
      g_queue_push_head(icache_ctrl->iblock_lru,iblock);
      g_mutex_unlock(icache_ctrl->icache_mutex);

      //HLOG_DEBUG("--read iblock no:%llu",iblock->iblock_no);
      g_assert(iblock_no == iblock->iblock_no);
      memcpy(iblock_buf, iblock->iblock,icache_ctrl->iblock_size);
      icache_ctrl->icache_hit++;
      //HLOG_DEBUG("--Leaving func %s", __func__);
      return ret;
}


