#include <string.h>
#include "cache.h"
#include "cache_helper.h"
#include "hlfs_log.h"

int cache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count, char *block_buf)
{
	HLOG_DEBUG("--Entering func :%s", __func__);
	int ret = 0;
#if 0
    printf("--need trigger flush worker to free cache block_count:%d,cache_size:%d,free_block:%d,min blocks:%ld--\n",
               block_count,
               cache_ctrl->cache_size,
               get_cache_free_size(cache_ctrl),
               (cache_ctrl->flush_trigger_level * cache_ctrl->cache_size)/100);
#endif
	if (block_count + cache_ctrl->cache_size - get_cache_free_size(cache_ctrl) 
            >= (cache_ctrl->flush_trigger_level * cache_ctrl->cache_size)/100) {
       HLOG_DEBUG("--wake up flush worker--");
       g_mutex_lock(cache_ctrl->cache_mutex);
       g_cond_signal(cache_ctrl->flush_waken_cond);
       g_mutex_unlock(cache_ctrl->cache_mutex);
       //printf("--free count:%d,block count:%d\n",get_cache_free_size(cache_ctrl),block_count);
       g_mutex_lock(cache_ctrl->cache_mutex);
       while (__get_cache_free_size(cache_ctrl) < block_count) {
           HLOG_DEBUG("--wait continue--");
           g_cond_wait(cache_ctrl->writer_waken_cond, cache_ctrl->cache_mutex);
       }
       HLOG_DEBUG("--continue to write--");
       g_mutex_unlock(cache_ctrl->cache_mutex);
    }
    HLOG_DEBUG("--write to cache start bno:%d,block_count:%d--", start_block_no, block_count);
    write_cache(cache_ctrl, start_block_no, block_count, block_buf); 
	return ret;
}


int cache_insert_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf){
    return cache_insert_blocks(cache_ctrl, block_no, 1, block_buf);
}
