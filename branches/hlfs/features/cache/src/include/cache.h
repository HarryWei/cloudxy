/**
 * HLFS cache provide cache for HLFS.
 * 
 * The cache related code is licensed under the terms of GPL v2 or later.
 * See COPYING in the DOCUMENTATION directory.
 * 
 * Copyright (C) 2012 XUPT
 * 
 * Authors:
 * 	Kelvin Wang 
 * 		<senwang@linux.vnet.ibm.com>
 * 		<kelvin.xupt@gmail.com>
 * 
 */

#ifndef __CACHE_H__
#define __CACHE_H__

#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef struct {
	uint64_t block_no;
	char *block;
} block_t;

struct cache_ctrl {
	GMutex *cache_mutex; 	//Lock of cache
	GTrashStack *block_cache; 	//Stack used to store cache buffers
	GQueue *dirty_block; 	//LRU queue of dirty blocks
	GHashTable *block_map; 	//Hash Map
	GThread *flush_worker; 	//Back end flush thread 
	GCond *flush_waken_cond; 	//The condition of Writer thread awaking flush thread
	GCond *writer_waken_cond; 	//The condition of flush thread awaking writer thread
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
