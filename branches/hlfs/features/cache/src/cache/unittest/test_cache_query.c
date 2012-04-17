/*
 * Unit test for cache query;
 * Copyright (c) XUPT
 * By Kelvin Wang<senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

typedef struct {
    CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;
block_t _block[100];
void case_setup()
{
	fixture.cache_ctrl = cache_new();
	int ret = 0;
	int i = 0;
	block_t *block = NULL;
    ret = cache_init(fixture.cache_ctrl, 8192, 1024, 100, 80, 1024);
	for (i = 1; i <= 100; i++) {
		block = (block_t *)g_trash_stack_pop(&fixture.cache_ctrl->block_cache);
		if (block == NULL) {
			g_message("No available cache left");
			return;
		}
		_block[i].block_no = block->block_no;
		_block[i].block = block->block;
		g_hash_table_insert(fixture.cache_ctrl->block_map, (gpointer)i, \
				(gpointer)block);
	}
}

void test_case_cache_query()
{
	uint64_t i = 0;
	char *__block = NULL;
	for (i = 1; i <= 100; i++) {
		if (0 > cache_query(fixture.cache_ctrl, i, &__block)) {
			g_message("query error");
			return;
		}
		g_assert(__block == _block[i].block); 
	}
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
