/*
 * Unit test for cache creation and initialization;
 * 
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
void case_setup()
{
	fixture.cache_ctrl = cache_new();
}

void test_case_cache_init()
{
	int ret = 0;
    ret = cache_init(fixture.cache_ctrl, 8192, 1024, 100, 80, 1024);
    g_assert(ret == 0);
	g_assert(fixture.cache_ctrl->block_size == 8192);
	g_assert(fixture.cache_ctrl->cache_size == 1024);
	g_assert(fixture.cache_ctrl->flush_interval == 100);
	g_assert(fixture.cache_ctrl->flush_trigger_level == 80);
	g_assert(fixture.cache_ctrl->flush_once_size == 1024);
	ret = g_trash_stack_height(&(fixture.cache_ctrl->block_cache));
	g_assert(ret == 1025);
	g_assert(fixture.cache_ctrl->dirty_block != NULL);
	g_assert(fixture.cache_ctrl->block_map != NULL);
}

void case_teardown()
{
	if (0 > cache_destroy(fixture.cache_ctrl)) {
		g_message("destroy cache error");
		return -1;
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
				test_case_cache_init, 
				case_teardown);
	
	return g_test_run();
}
