#ifndef __CACHE_HLEPER_H__
#define __CACHE_HLEPER_H__
#include "api/hlfs.h"
#include "glib.h"
#include "cache.h"

#ifdef __cplusplus  
extern "C" {
#endif

int write_cache(CACHE_CTRL *cctrl, uint32_t start_block_no,char *block_buf);
uint32_t __get_cache_free_size(CACHE_CTRL *cctrl);
uint32_t get_cache_free_size(CACHE_CTRL *cctrl);
void __free_from_cache(CACHE_CTRL *cctrl, GSList *free_list);
void free_from_cache(CACHE_CTRL *cctrl, GSList *free_list);
int get_continues_blocks(CACHE_CTRL *cctrl, GSList **continue_block_list);
block_t * cache_query(CACHE_CTRL *cache_ctrl,uint64_t block_no);
#ifdef __cplusplus 
} 
#endif 

#endif
