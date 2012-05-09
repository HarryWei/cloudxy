#include "glib.h"
#include "icache.h"
ICACHE_CTRL *icache_new(){
    HLOG_DEBUG("--Entering func %s", __func__);
	struct icache_ctrl *icache_ctrl = NULL;
	if (NULL == (icache_ctrl = (struct icache_ctrl *)g_malloc0(sizeof(struct \
						icache_ctrl)))) {
		HLOG_ERROR("--Error:Apply for mem");
		return NULL;
	}
    return cache_ctrl;
}
int icache_init(ICACHE_CTRL *icache_ctrl,
		uint64_t iblock_size,
		uint64_t icache_size,
		uint64_t invalidate_trigger_level,
		uint64_t invalidate_once_size){
    HLOG_DEBUG("--Entering func %s", __func__);
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
    HLOG_DEBUG("--cache container init over!--");
	
	if (NULL == (icache_ctrl->iblock_lru = 	g_queue_new())) {
		HLOG_ERROR("--Error:Apply for LRU queue");
		ret = -EHLFS_MEM;
		goto err;
    }	
    HLOG_DEBUG("--lru block queue init over!--");
	if (NULL == (icache_ctrl->iblock_map = g_hash_table_new(g_int64_hash,g_int64_equal))) {
		HLOG_ERROR("--Error:Apply for block_map");
		ret = -EHLFS_MEM;
		goto err;
    }
    HLOG_DEBUG("--iblock_map init over!--");
    g_thread_init(NULL);
    if (NULL == (icache_ctrl->icache_mutex = g_mutex_new())) {
        HLOG_ERROR("--Error:Apply for mutext");
        ret = -EHLFS_MEM;
        goto err;
    }
	HLOG_DEBUG("--Leaving func %s", __func__);
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
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}

int icache_destroy(ICACHE_CTRL *icache_ctrl){
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
			HLOG_DEBUG("g_trash_stack_height: %d", g_trash_stack_height(&cache_ctrl->iblock_cache));
			iblock_t *_iblock = (iblock_t *)g_trash_stack_pop(&icache_ctrl->iblock_cache);
			if (_iblock->iblock != NULL) 
				g_free(_iblock->iblock);
			g_free(_iblock);
			i++;
			HLOG_DEBUG("----destroy %d succ", i);
        }
    }
    g_mutex_clear (icache_ctrl->icache_mutex);
  
	g_free(icache_ctrl);
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
    return 0;
}


//int icache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int icache_insert_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf){
    HLOG_DEBUG("--enter fun %s", __func__);
	icache_ctrl->total_write_count++;
    g_mutex_lock(icache_ctrl->icache_mutex);
	if(g_queue_get_length(icache_ctrl->iblock_lru) =< icache_ctrl->invalidate_once_size)){
	   int count = icache_ctrl->invalidate_once_size);
	   while(count --){
	   	   iblock_t * iblock = g_queue_pop_tail(icache_ctrl->iblock_lru);
		   g_hash_table_remove(icache_ctrl->iblock_map,GUINT_TO_POINTER(iblock->iblock_no));
	       g_trash_stack_push(&icache_ctrl->iblock_cache,iblock);
	   }
	}	
    iblock_t *iblock = g_trash_stack_pop(&icache_ctrl->iblock_cache);
    iblock->iblock_no = iblock_no;
    memcpy(iblock->iblock, iblock_buf , icache_ctrl->iblock_size);
    g_hash_table_insert(icache_ctrl->iblock_map, GUINT_TO_POINTER(iblock->iblock_no),iblock);
	g_queue_push_head(icache_ctrl->iblock_lru,iblock);
    g_mutex_unlock(icache_ctrl->icache_mutex);
	return 0;
}

int icache_query_iblock(ICACHE_CTRL *icache_ctrl, uint64_t iblock_no, char *iblock_buf){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	icache_ctrl->total_read_count++;
	g_mutex_lock(icache_ctrl->icache_mutex);
	iblock_t * iblock  = (iblock_t*)g_hash_table_lookup(icache_ctrl->iblock_map, \
			GUINT_TO_POINTER(iblock_no));
	if (iblock == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("NO item in hash table");
		return ret;
	}
    g_queue_remove(icache_ctrl->iblock_lru,iblock);
	g_queue_push_head(icache_ctrl->iblock_lru,iblock);
    g_mutex_unlock(icache_ctrl->icache_mutex);
	
	HLOG_DEBUG("--read iblock no:%llu",iblock->iblock_no);
	g_assert(iblock_no == iblock->iblock_no);
	memcpy(iblock_buf, iblock->block, (size_t)icache_ctrl->iblock_size);
	icache_ctrl->icache_hit++;
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;
}

gboolean  icache_iblock_exist(ICACHE_CTRL *icache_ctrl, uint64_t iblock_no){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    iblock_t * iblock = icache_query_iblock(icache_ctrl,iblock_no);
    if(iblock==NULL){
       return FALSE;   
    }
    return TRUE;
}

