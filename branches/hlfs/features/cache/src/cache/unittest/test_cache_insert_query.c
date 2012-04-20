/*
 * Unit test for cache insert;
 * Copyright (c) XUPT
 * By Kelvin Wang<senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

#define BLOCK_SIZE 8192
typedef struct { 
	CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;
void case_setup()
{
	g_message("--enter fun %s", __func__);
	fixture.cache_ctrl = cache_new();
    cache_init(fixture.cache_ctrl, BLOCK_SIZE, 1024, 100, 80, 1024);
}

void test_case_cache_insert()
{
	g_message("--enter fun %s", __func__);
	int ret = 0;
	uint64_t i = 2;
	char *_block = NULL;
	char *__block = NULL;

	_block = (char *)g_malloc0(BLOCK_SIZE);
	__block = (char *)g_malloc0(BLOCK_SIZE);
	sprintf(_block, "hello cache");
	ret = cache_insert_block(fixture.cache_ctrl, i, _block);
	g_assert(ret == 0);
	g_message("inserted,now test");
	ret = cache_query_block(fixture.cache_ctrl, i, &__block);
	
	g_message("If hello cache printed, we test successfully");
	g_message("waiting...");
	g_message("%s\n", __block);
	g_message("Got it");
	g_free(_block);
	g_free(__block);
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
				test_case_cache_insert, 
				case_teardown);
	
	return g_test_run();
}
