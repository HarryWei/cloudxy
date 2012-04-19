/*
 * Unit test for cache query;
 * Copyright (c) XUPT
 * By Kelvin Wang<senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

#define HASH_LEN 10
typedef struct { 
	CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;
void case_setup()
{
	g_message("--enter fun %s", __func__);
	fixture.cache_ctrl = cache_new();
	int ret = 0;
	uint64_t i = 0;
	block_t *block = NULL;
    ret = cache_init(fixture.cache_ctrl, 8192, 1024, 100, 80, 1024);
	for (i = 0; i < HASH_LEN; i++) {
		block = (block_t *)g_trash_stack_pop(&fixture.cache_ctrl->block_cache);
		if (block == NULL) {
			g_message("No available cache left");
			return;
		}
		block->block_no = i;
		g_message("block no: %llu\nblock addr: %p", block->block_no, block->block);
		g_hash_table_insert(fixture.cache_ctrl->block_map, (gpointer)&block->block_no, \
				(gpointer)block);
		g_message("insert %llu succ");
	}
	g_message("--leave fun %s", __func__);
}

void test_case_cache_query()
{
	g_message("--enter fun %s", __func__);
	int ret = 0;
	uint64_t i = 2;
	char *_block = NULL;
	_block = (char *)g_malloc0(fixture.cache_ctrl->block_size);
	ret = cache_query(fixture.cache_ctrl, i, &_block);
	g_assert(ret == 0);
	g_assert(_block != NULL); 
	g_message("block no: %llu block addr: %p", i, _block);
	g_message("--leave fun %s", __func__);
}

void case_teardown()
{
	if (0 > cache_destroy(fixture.cache_ctrl)) {
		g_message("destroy cache error");
		return;
	}	
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/cache_init", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_query, 
				case_teardown);
	
	return g_test_run();
}
