/*
 * Unit test for cache creation and initialization;
 * 
 * By Kelvin Wang <senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

typedef struct {
    CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;
void case_setup()
{
	fixture.cache_ctrl = cache_new();
    g_assert(fixture.cache_ctrl != NULL);
}

void test_cache_init()
{
	int ret = 0;
    ret = cache_init(fixture.cache_ctrl, 8192, 1024, 10, 80, 1024);
    g_assert(ret == 0);
	g_assert(fixture.cache_ctrl->block_size == 8192);
	g_assert(fixture.cache_ctrl->cache_size == 1024);
	g_assert(fixture.cache_ctrl->flush_interval == 10);
	g_assert(fixture.cache_ctrl->flush_trigger_level == 80);
	g_assert(fixture.cache_ctrl->flush_once_size == 1024);
	ret = g_trash_stack_height(&(fixture.cache_ctrl->block_cache));
	g_assert(ret == 1024);
	g_assert(fixture.cache_ctrl->dirty_block != NULL);
    g_assert(g_queue_get_length(fixture.cache_ctrl->dirty_block) == 0);
	g_assert(fixture.cache_ctrl->block_map != NULL);
	g_assert(fixture.cache_ctrl->flush_waken_cond != NULL);
	g_assert(fixture.cache_ctrl->writer_waken_cond != NULL);
}

void case_teardown()
{
    if (fixture.cache_ctrl->block_cache) {
		int i = 0;
        while (g_trash_stack_height(&fixture.cache_ctrl->block_cache) != 0) {
			g_message("g_trash_stack_height: %d", g_trash_stack_height(&fixture.cache_ctrl->block_cache));
			block_t *_block = (block_t *)g_trash_stack_pop(&fixture.cache_ctrl->block_cache);
			if (_block->block != NULL) 
				g_free(_block->block);
			g_free(_block);
			i++;
			g_message("----destroy %d succ", i);
        }
    }
	if (fixture.cache_ctrl->flush_worker_should_exit == 0)
		fixture.cache_ctrl->flush_worker_should_exit = 1;
	g_thread_join(fixture.cache_ctrl->flush_worker);
	if (fixture.cache_ctrl->block_map != NULL) {
		g_hash_table_destroy(fixture.cache_ctrl->block_map);
	}
	if (fixture.cache_ctrl->dirty_block != NULL) {
    	g_queue_free(fixture.cache_ctrl->dirty_block);
	}
	if (fixture.cache_ctrl->flush_waken_cond != NULL) {
		g_cond_free(fixture.cache_ctrl->flush_waken_cond);
	}
	if (fixture.cache_ctrl->writer_waken_cond != NULL) {
		g_cond_free(fixture.cache_ctrl->writer_waken_cond);
	}
	g_free(fixture.cache_ctrl);
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
				test_cache_init, 
				case_teardown);
	return g_test_run();
}
