/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *  Maintainers : Kelvin Wang 
  *       <senwang@linux.vnet.ibm.com>
  *       <kelvin.xupt@gmail.com>
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */


#ifndef __CACHE_H__
#define __CACHE_H__

//#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef struct {
	uint32_t block_no;
	char *block;
} block_t;


 #define DEF_CACHE_SIZE      4096U
 #define DEF_FLUSH_INTERVAL  5U
 #define DEF_FLUSH_TRIGGER_LEVEL  80U
 #define DEF_FLUSH_ONCE_SIZE   64U


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
	uint32_t cache_size; 	//Number of cache buffers
	uint32_t block_size; 	//Size of each buffer
	uint32_t flush_interval; 
	uint32_t flush_trigger_level; 	//Percentage 
	uint32_t flush_once_size; 	//Number of dirty blocks have been written one time 
	uint64_t total_write_count; 
	uint64_t total_read_count;
#if 0
	uint64_t cache_full_hit; 
	uint64_t cache_part_hit;
#else
    uint64_t cache_hit;
#endif 
}CACHE_CTRL;

CACHE_CTRL *cache_new(void);
int cache_init(CACHE_CTRL *cache_ctrl,
		uint32_t block_size,
		uint32_t cache_size,
		uint32_t flush_interval,
		uint32_t flush_trigger_level,
		uint32_t flush_once_size);
int cache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int cache_insert_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf);
int cache_query_block(CACHE_CTRL *cache_ctrl, uint32_t block_no, char *block_buf);
gboolean  cache_block_exist(CACHE_CTRL *cache_ctrl, uint32_t block_no);
int cache_set_write_cb(CACHE_CTRL *cache_ctrl, void *cb_func, void * cb_user_param);
int cache_destroy(CACHE_CTRL *cache_ctrl);
int cache_sync(CACHE_CTRL *cache_ctrl);
#ifdef __cplusplus 
} 
#endif 
#endif
