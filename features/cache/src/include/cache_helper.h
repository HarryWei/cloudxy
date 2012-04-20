#ifndef __CACHE_HLEPER_H__
#define __CACHE_HLEPER_H__
#include "api/hlfs.h"
#include "glib.h"
#include "cache.h"

static int write_cache(CACHE_CTRL *cctrl,uint64_t start_block_no,uint32_t block_count,char*block_buf){
    g_mutex_lock(cctrl->cache_mutex);
    int i;
    int idx=0;
    for(i=start_block_no;i<start_block_no+block_count;i++,idx++){
        block_t * block = g_trash_stack_pop(&cctrl->block_cache);
        block->block_no = i;
        memcpy(block->block,block_buf+idx*cctrl->block_size,cctrl->block_size);
        g_hash_table_insert(cctrl->block_map,&block->block_no,block);
        g_queue_push_tail(cctrl->dirty_block,block);
    }
    g_mutex_unlock(cctrl->cache_mutex);
    return block_count;
}
static uint32_t get_cache_free_size(CACHE_CTRL *cctrl){
    g_mutex_lock(cctrl->cache_mutex);
    int used_block_size = g_queue_get_length(cctrl->dirty_block);
    g_mutex_unlock(cctrl->cache_mutex);
    uint32_t free_block_size = cctrl->cache_size - used_block_size;
    return free_block_size;
}

static void __free_from_cache(CACHE_CTRL *cctrl,GSList *free_list){
    int free_block_count =  g_slist_length(free_list);
    int i;
    for(i=0;i<free_block_count;i++){
        block_t *block = g_slist_nth_data(free_list,i);  
        g_hash_table_remove(cctrl->block_map,&block->block_no);
        g_queue_pop_head(cctrl->dirty_block);
        g_trash_stack_push(&cctrl->block_cache,block);
    }
}

static void free_from_cache(CACHE_CTRL *cctrl,GSList *free_list){
    g_mutex_lock(cctrl->cache_mutex);
    __free_from_cache(cctrl,free_list);
    g_mutex_unlock(cctrl->cache_mutex);
}

static int get_continues_blocks(CACHE_CTRL *cctrl,GSList *continue_block_list){
    HLOG_DEBUG("--enter func %s--",__func__);
    g_mutex_lock(cctrl->cache_mutex);
    int size = g_queue_get_length(cctrl->dirty_block);
    block_t *block = (gpointer)g_queue_peek_head(cctrl->dirty_block);
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
           continue_block_list = g_slist_append(continue_block_list,block);
        }
        if(block->block_no == min_block_no - 1){
           HLOG_DEBUG("--find continue block no:%lu--",block->block_no);
           min_block_no--;
           continue_block_list = g_slist_prepend(continue_block_list,block);
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

