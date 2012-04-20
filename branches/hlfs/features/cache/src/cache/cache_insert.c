#include <string.h>
#include "cache.h"
#include "cache_helper.h"
#include "hlfs_log.h"

int cache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    if(block_count + cache_ctrl->cache_size - get_cache_free_size(cache_ctrl) 
            < cache_ctrl->flush_trigger_level * cache_ctrl->cache_size){
       HLOG_DEBUG("--need trigger flush worker to free cache--");
       g_mutex_lock(cache_ctrl->cache_mutex);
       g_cond_signal(cache_ctrl->flush_waken_cond);
       g_mutex_unlock(cache_ctrl->cache_mutex);

       g_mutex_lock(cache_ctrl->cache_mutex);
       while(get_cache_free_size(cache_ctrl) < block_count)
           g_cond_wait(cache_ctrl->writer_waken_cond,cache_ctrl->cache_mutex);
       g_mutex_unlock(cache_ctrl->cache_mutex);
    }else{
       HLOG_DEBUG("--write to cache--");
       write_cache(cache_ctrl,start_block_no,block_count,block_buf); 
    }
	return ret;
}


int cache_insert_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf){
    return cache_insert_blocks(cache_ctrl,block_no,1,block_buf);
}
