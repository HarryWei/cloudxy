#ifndef __CACHE_HLEPER_H__
#define __CACHE_HLEPER_H__
#include "api/hlfs.h"
#include "glib.h"
#include "cache.h"

static int write_cache(CACHE_CTRL *cctrl, uint64_t start_block_no,char *block_buf) {
	HLOG_DEBUG("--enter fun %s", __func__);
    g_mutex_lock(cctrl->cache_mutex);
    block_t *block = g_trash_stack_pop(&cctrl->block_cache);
    block->block_no = start_block_no;
    memcpy(block->block, block_buf , cctrl->block_size);
    g_hash_table_insert(cctrl->block_map, GUINT_TO_POINTER(block->block_no), block);
    g_queue_push_tail(cctrl->dirty_block, block);
    g_mutex_unlock(cctrl->cache_mutex);
    return 0;
}

static uint32_t __get_cache_free_size(CACHE_CTRL *cctrl){
	 HLOG_DEBUG("--enter fun %s", __func__);
     uint32_t free_block_size = cctrl->cache_size - g_queue_get_length(cctrl->dirty_block);
	 HLOG_DEBUG("--free block size:%d",free_block_size);
     return free_block_size;
}

static uint32_t get_cache_free_size(CACHE_CTRL *cctrl) {
    g_mutex_lock(cctrl->cache_mutex);
    uint32_t free_block_size = __get_cache_free_size(cctrl);
    g_mutex_unlock(cctrl->cache_mutex);
    return free_block_size;
}

static void __free_from_cache(CACHE_CTRL *cctrl, GSList *free_list) {
	HLOG_DEBUG("--enter fun %s", __func__);
    int free_block_count =  g_slist_length(free_list);
    int i;
    for(i=0;i<free_block_count;i++){
        block_t *block = g_slist_nth_data(free_list,i);  
        g_hash_table_remove(cctrl->block_map,&block->block_no);
        g_queue_pop_head(cctrl->dirty_block);
        g_trash_stack_push(&cctrl->block_cache,block);
    }
}

static void free_from_cache(CACHE_CTRL *cctrl, GSList *free_list) {
    g_mutex_lock(cctrl->cache_mutex);
    __free_from_cache(cctrl,free_list);
    g_mutex_unlock(cctrl->cache_mutex);
}

static int get_continues_blocks(CACHE_CTRL *cctrl, GSList **continue_block_list){
    HLOG_DEBUG("--enter func %s--",__func__);
    g_mutex_lock(cctrl->cache_mutex);
    int size = g_queue_get_length(cctrl->dirty_block);
    if (size == 0) {
       HLOG_DEBUG("-- not find any dirty block --");
       g_mutex_unlock(cctrl->cache_mutex);
       return 0;
    }
    if (*continue_block_list != NULL) {
       HLOG_DEBUG("-- continues block list should be NULL before start --");
       g_mutex_unlock(cctrl->cache_mutex);
       return -1;
    }
    block_t *block = (gpointer)g_queue_peek_head(cctrl->dirty_block);
    int max_block_no = block->block_no;
    int min_block_no = block->block_no;
    HLOG_DEBUG("--total dirty block:%d,oldest block no:%llu--",size,block->block_no);
    *continue_block_list = g_slist_append(*continue_block_list, block);
    if (size == 1) {
       g_mutex_unlock(cctrl->cache_mutex);
       return 0; 
    }
    uint64_t count = 0;
    int idx = 1; 
    while (TRUE) {
        block_t *block = (gpointer)g_queue_peek_nth(cctrl->dirty_block, idx);
        g_assert(block != NULL);
        if (max_block_no + 1 != block->block_no && min_block_no - 1 != block->block_no) {
           HLOG_DEBUG("--not find continue block no:%llu!--",block->block_no);
           break;
        }
        if (block->block_no == max_block_no + 1) {
           HLOG_DEBUG("--find after continue block no:%llu--",block->block_no);
           max_block_no++;
           *continue_block_list = g_slist_append(*continue_block_list, block);
        }
        if (block->block_no == min_block_no - 1) {
           HLOG_DEBUG("--find before continue block no:%llu--",block->block_no);
           min_block_no--;
           *continue_block_list = g_slist_prepend(*continue_block_list, block);
        }
        if (++count >= cctrl->flush_once_size - 1) {
           HLOG_DEBUG("--get enought flush once count block :%llu--",cctrl->flush_once_size);
           break;
        }
        if (++idx > size - 1) {
           HLOG_DEBUG("--iterator over :%d--",idx);
           break;
        }
    }
    g_mutex_unlock(cctrl->cache_mutex);
    return 0;
}

static block_t * cache_query(CACHE_CTRL *cache_ctrl,uint64_t block_no){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
	block_t *block = NULL;
	if (cache_ctrl == NULL) {
		ret = -EHLFS_PARAM;
		HLOG_ERROR("param error");
		return NULL;
	}

	if (0 == cache_ctrl->cache_size - get_cache_free_size(cache_ctrl)) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("The hash table of block_map is empty");
		return NULL;
	}
	
	HLOG_DEBUG("block_no %llu will be queried",block_no);
    g_mutex_lock(cache_ctrl->cache_mutex);
	block = (block_t*)g_hash_table_lookup(cache_ctrl->block_map, \
			GUINT_TO_POINTER(block_no));
    g_mutex_unlock(cache_ctrl->cache_mutex);
    return block;
}
#endif
