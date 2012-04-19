#include "cache.h"
#include "string.h"

int cache_insert(CACHE_CTRL *cache_ctrl, uint64_t block_no, char *block)
{
	HLOG_DEBUG("--Entering func %s", __func__);
	int ret = 0;
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
	return ret;
}
