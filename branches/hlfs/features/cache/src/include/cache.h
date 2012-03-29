#ifndef _CACHE_H
#define _CACHE_H

#include "api/hlfs.h"
#include "glib.h"

typedef struct {
	uint64_t block_no;
	char *block;
} block_t;

struct cache_ctrl {
	GMutex *cache_mutex; 	//Lock of cache
	GTrashStack *block_cache; 	//Stack used to store cache buffers
	GQueue *dirty_block; 	//LRU queue of dirty blocks
	Ghash *block_map; 	//
	Gthread *flush_worker; 	//Back end flush thread 
	Gcond *flush_waken_cond; 	//The condition of Writer thread awaking flush thread
	Gcond *writer_waken_cond; 	//The condition of flush thread awaking writer thread
	void *write_callback_func; 	//
	void *write_callback_user_param; 	//
	uint64_t cache_size; 	//Number of cache buffers
	uint64_t block_size; 	//Size of each buffer
	uint64_t flush_interval; 
	uint64_t flush_trigger_level; 	//Percentage
	uint64_t flush_once_size; 	//Number of dirty blocks have been written one time
	uint64_t total_write_count;
	uint64_t total_read_count;
	uint64_t cache_full_hit; 
	uint64_t cache_part_hit;
};


#endif
