#ifndef __CACHE_HLEPER_H__
#define __CACHE_HLEPER_H__
#include "api/hlfs.h"
#include "glib.h"
#include "cache.h"

static uint32_t get_cache_free_size(CACHE_CTRL *cctrl){
    return 0;
}
static void free_from_cache(CACHE_CTRL *cctrl,GSList *free_list){
    return;
}
static int get_continues_blocks(CACHE_CTRL *cctrl,GSList *continue_block_list){
    HLOG_DEBUG("--enter func %s--",__func__);
    g_mutex_lock(cctrl->cache_mutex);
    int size = g_queue_get_length(cctrl->dirty_block);
    block_t *block = (gpointer)g_queue_pop_head(cctrl->dirty_block);
    int max_block_no = block->block_no;
    int min_block_no = block->block_no;
    HLOG_DEBUG("--oldest block no:%lu--",block->block_no);
    uint64_t count=0;
    while(TRUE){
        int tail = size - count; 
        block_t *block = (gpointer)g_queue_peek_nth(cctrl->dirty_block,tail);
        if(max_block_no+1!=block->block_no || min_block_no-1!=block->block_no){
           HLOG_DEBUG("--not find continue block no:%lu--",block->block_no);
           break;
        }
        if(block->block_no == max_block_no + 1){
           HLOG_DEBUG("--find continue block no:%lu--",block->block_no);
           max_block_no++;
           g_slist_append(continue_block_list,block);
        }
        if(block->block_no == min_block_no - 1){
           HLOG_DEBUG("--find continue block no:%lu--",block->block_no);
           min_block_no--;
           g_slist_prepend(continue_block_list,block);
        }
        count++;
        if(count >= cctrl->flush_once_size){
           HLOG_DEBUG("--get enought flush once count block :%lu--",cctrl->flush_once_size);
           break;
        }
    }
    g_mutex_unlock(cctrl->cache_mutex);
    return 0;
}
#endif

