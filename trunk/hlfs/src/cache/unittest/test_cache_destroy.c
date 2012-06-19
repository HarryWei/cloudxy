/*
 * Unit test for cache destroy;
 * 
 * By Kelvin Wang<senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

#define HASH_NUM 100

typedef struct {
    CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;
void case_setup()
{
	int ret = 0;
	fixture.cache_ctrl = cache_new();
    g_assert(fixture.cache_ctrl != NULL);
    cache_init(fixture.cache_ctrl, 8192, 1024, 20, 80, 1024);
	g_assert(fixture.cache_ctrl != NULL);
	g_assert(ret == 0);
}

void test_cache_destroy()
{
	int ret = 0;
	uint64_t i = 0;

	for (i = 0; i < HASH_NUM; i++) {
		block_t *_block = g_trash_stack_pop(&fixture.cache_ctrl->block_cache);
		g_hash_table_insert(fixture.cache_ctrl->block_map, (gpointer)&i, \
				(gpointer)_block);
		g_message("inserting key: %llu, value: %p", i, _block);
		g_queue_push_tail(fixture.cache_ctrl->dirty_block, (gpointer)_block);
	}

	ret = cache_destroy(fixture.cache_ctrl);
	g_assert(ret == 0);
}

void case_teardown()
{
}


int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/cache_destroy", 
				Fixture, 
				NULL,
				case_setup, 
				test_cache_destroy, 
				case_teardown);
	return g_test_run();
}
