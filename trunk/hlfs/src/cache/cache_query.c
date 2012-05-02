#include "cache.h"
#include "cache_helper.h"

int cache_query_block(CACHE_CTRL *cache_ctrl, uint64_t block_no, char *block_buf){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    cache_ctrl->total_read_count++;
    block_t * block = cache_query(cache_ctrl,block_no);
	if (block == NULL) {
		ret = -EHLFS_NOITEM;
		HLOG_ERROR("NO item in hash table");
		return ret;
	}
	HLOG_DEBUG("--read block no:%llu",block->block_no);
	g_assert(block_no == block->block_no);
	memcpy(block_buf, block->block, (size_t)cache_ctrl->block_size);
    cache_ctrl->cache_hit++;
	HLOG_DEBUG("--Leaving func %s", __func__);
	return ret;

}
gboolean  cache_block_exist(CACHE_CTRL *cache_ctrl, uint64_t block_no){
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
    block_t * block = cache_query(cache_ctrl,block_no);
    if(block==NULL){
       return FALSE;   
    }
    return TRUE;
}
