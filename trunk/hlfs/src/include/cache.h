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

//#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef struct {
	uint64_t block_no;
	char *block;
} block_t;


 #define DEF_CACHE_SIZE      4096
 #define DEF_FLUSH_INTERVAL  5
 #define DEF_FLUSH_TRIGGER_LEVEL  80
 #define DEF_FLUSH_ONCE_SIZE   64


#ifdef __cplusplus  
extern "C" {
#endif
typedef int (*FLUSH_CB)(void* user_data, char *block_buf, uint32_t start_no,uint32_t end_no);
typedef struct cache_ctrl {
	GMutex *cache_mutex; 	//Lock of cache
	GTrashStack *block_cache; 	//Stack used to store cache buffers
	GQueue *dirty_block; 	//LRU queue of dirty blocks
	GHashTable *block_map; 	//Hash Map
	GThread *flush_worker; 	//Back end flush thread 
    uint32_t flush_worker_should_exit;
	GCond *flush_waken_cond; 	//The condition of Writer thread awaking flush thread
	GCond *writer_waken_cond; 	//The condition of flush thread awaking writer thread
	FLUSH_CB write_callback_func; 	//
	void *write_callback_user_param;//
	uint64_t cache_size; 	//Number of cache buffers
	uint64_t block_size; 	//Size of each buffer
	uint64_t flush_interval; 
	uint64_t flush_trigger_level; 	//Percentage 
	uint64_t flush_once_size; 	//Number of dirty blocks have been written one time 
	uint64_t total_write_count; 
	uint64_t total_read_count;
#if 0
	uint64_t cache_full_hit; 
	uint64_t cache_part_hit;
#else
    uint64_t cache_hit;
#endif 
}CACHE_CTRL;

CACHE_CTRL *cache_new();
int cache_init(CACHE_CTRL *cache_ctrl,
		uint64_t block_size,
		uint64_t cache_size,
		uint64_t flush_interval,
		uint64_t flush_trigger_level,
		uint64_t flush_once_size);
int cache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int cache_insert_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf);
int cache_query_block(CACHE_CTRL *cache_ctrl, uint64_t block_no, char *block_buf);
gboolean  cache_block_exist(CACHE_CTRL *cache_ctrl, uint64_t block_no);
int cache_set_write_cb(CACHE_CTRL *cache_ctrl, void *cb_func, void * cb_user_param);
int cache_destroy(CACHE_CTRL *cache_ctrl);
int cache_sync(CACHE_CTRL *cache_ctrl);
#ifdef __cplusplus 
} 
#endif 
#endif
