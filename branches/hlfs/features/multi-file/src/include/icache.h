/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __ICACHE_H__
#define __ICACHE_H__

//#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef struct {
	uint32_t iblock_no;
	char *   iblock;
} iblock_t;


#define DEF_ICACHE_SIZE 1024U
#define DEF_INVALIDATE_TRIGGER_LEVEL 90U
#define DEF_INVALIDATE_ONCE_SIZE 64U


#ifdef __cplusplus  
extern "C" {
#endif

typedef struct icache_ctrl {
	GMutex		*icache_mutex; 	//Lock of cache
	GTrashStack	*iblock_cache; 	//Stack used to store cache buffers
	GQueue		*iblock_lru; 	//LRU queue of iblock
	GHashTable	*iblock_map; 	//Hash Map
	uint32_t 	       icache_size; 	//Number of cache buffers
	uint32_t 	       iblock_size; 	//Size of each buffer
	uint32_t		invalidate_trigger_level; 	// 
	uint32_t		invalidate_once_size; 	//Number of dirty blocks have been written one time 
	uint64_t 		total_write_count; 
	uint64_t 		total_read_count;
    uint64_t 		icache_hit;
}ICACHE_CTRL;

ICACHE_CTRL *icache_new(void);
int icache_init(ICACHE_CTRL *icache_ctrl,
		uint32_t iblock_size,
		uint32_t icache_size,
		uint32_t invalidate_trigger_level,
		uint32_t invalidate_once_size);
//int icache_insert_blocks(CACHE_CTRL *cache_ctrl, uint32_t start_block_no, uint32_t block_count,char *block_buf);
int icache_insert_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf);
iblock_t * icache_query(ICACHE_CTRL *icache_ctrl,uint32_t iblock_no);
int icache_query_iblock(ICACHE_CTRL *icache_ctrl, uint32_t iblock_no, char *iblock_buf);
//gboolean  icache_iblock_exist(ICACHE_CTRL *icache_ctrl, uint64_t iblock_no);
int icache_destroy(ICACHE_CTRL *icache_ctrl);


#ifdef __cplusplus 
} 
#endif 
#endif

