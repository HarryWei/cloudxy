#ifndef __ICACHE_H__
#define __ICACHE_H__

//#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef struct {
	uint64_t iblock_no;
	char *   iblock;
} iblock_t;

#ifdef __cplusplus  
extern "C" {
#endif

typedef struct icache_ctrl {
	GMutex		*icache_mutex; 	//Lock of cache
	GTrashStack	*iblock_cache; 	//Stack used to store cache buffers
	GQueue		*dirty_block; 	//LRU queue of dirty blocks
	GHashTable	*iblock_map; 	//Hash Map
	uint64_t 	       icache_size; 	//Number of cache buffers
	uint64_t 	       iblock_size; 	//Size of each buffer
	uint64_t		invalidate_trigger_level; 	// 
	uint64_t		invalidate_once_size; 	//Number of dirty blocks have been written one time 
	uint64_t 		total_write_count; 
	uint64_t 		total_read_count;
    uint64_t 		icache_hit;
}ICACHE_CTRL;

ICACHE_CTRL *icache_new();
int icache_init(ICACHE_CTRL *icache_ctrl,
		uint64_t iblock_size,
		uint64_t icache_size,
		uint64_t invalidate_trigger_level,
		uint64_t invalidate_once_size);
//int icache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int icache_insert_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf);
int icache_query_iblock(ICACHE_CTRL *icache_ctrl, uint64_t iblock_no, char *iblock_buf);
gboolean  icache_iblock_exist(ICACHE_CTRL *icache_ctrl, uint64_t iblock_no);
int lcache_destroy(ICACHE_CTRL *icache_ctrl);


#ifdef __cplusplus 
} 
#endif 
#endif

