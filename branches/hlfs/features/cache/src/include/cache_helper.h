#ifndef __CACHE_HLEPER_H__
#define __CACHE_HLEPER_H__
#include "api/hlfs.h"
#include "glib.h"
#include "cache.h"

static uint32_t get_cache_free_size(CACHE_CTRL *cctrl){
    return 0;
}
static void free_from_cache(CACHE_CTRL *cctrl,GSList *free_list){
    return;
}
static int get_continues_blocks(CACHE_CTRL *cctrl,GSList *continues_block_list){
    return 0;
}
#endif

