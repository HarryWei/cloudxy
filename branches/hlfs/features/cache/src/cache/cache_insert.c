#include <string.h>
#include "cache.h"
<<<<<<< .mine
#include "cache_helper.h"
#include "hlfs_log.h"
=======
#include "string.h"
>>>>>>> .r1101

int cache_insert(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
<<<<<<< .mine
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
=======
	block_t *_block = NULL;
	
	if (block == NULL) {
		HLOG_ERROR("block NULL");
		ret = -EHLFS_PARAM;
		return ret;
	}

	_block = (block_t *)g_trash_stack_pop(&cache_ctrl->block_cache);
	if (_block == NULL) {
		HLOG_ERROR("_block(pop) NULL");
		ret = -EHLFS_NOITEM;
		return ret;
	}

	memcpy(_block->block, block, strlen(block));
	HLOG_DEBUG("block string: %s", _block->block);
	_block->block_no = block_no;
	HLOG_DEBUG("block_no:%llu block addr:%p will be inserted", _block->block_no, _block->block);

	g_hash_table_insert(cache_ctrl->block_map, (gpointer)&_block->block_no, \
			(gpointer)_block->block);
	HLOG_DEBUG("block_no: %llu inserted", _block->block_no);
	HLOG_DEBUG("--Leaving func %s", __func__);
>>>>>>> .r1101
	return ret;
}
