#include "glib.h"
#include "icache.h"

iblock_t * icache_query(ICACHE_CTRL *icache_ctrl,uint64_t iblock_no){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	iblock_t *iblock = NULL;
	if (icache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param error");
		return NULL;
	}
	HLOG_DEBUG("block_no %llu will be queried",iblock_no);
    g_mutex_lock(icache_ctrl->icache_mutex);
	iblock = (iblock_t*)g_hash_table_lookup(icache_ctrl->iblock_map,&(iblock_no));
    g_mutex_unlock(icache_ctrl->icache_mutex);
    return iblock;
}

//int icache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int icache_insert_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf){
    HLOG_DEBUG("--enter fun %s", __func__);
    icache_ctrl->total_write_count++;
    iblock_t *iblock = icache_query(icache_ctrl,iblock_no);
    if(iblock!=NULL){
        HLOG_DEBUG("--update exist iblock--");
        memcpy(iblock->iblock,iblock_buf,icache_ctrl->iblock_size);  
	    return 0;
    }
    g_mutex_lock(icache_ctrl->icache_mutex);
	if((icache_ctrl->icache_size - g_queue_get_length(icache_ctrl->iblock_lru)) < icache_ctrl->invalidate_once_size){
       HLOG_DEBUG("--invalidate block--");
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
    iblock = g_trash_stack_pop(&icache_ctrl->iblock_cache);
    HLOG_DEBUG("--insert iblock_no:%d--",iblock_no);
    iblock->iblock_no = iblock_no;
    memcpy(iblock->iblock, iblock_buf , icache_ctrl->iblock_size);
    g_hash_table_insert(icache_ctrl->iblock_map,&(iblock->iblock_no),iblock);
    g_queue_push_head(icache_ctrl->iblock_lru,iblock);
    g_mutex_unlock(icache_ctrl->icache_mutex);
   return 0;
}


