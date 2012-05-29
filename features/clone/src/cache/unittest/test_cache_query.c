/*
 * Unit test for cache query;
 * Copyright (c) XUPT
 * By Kelvin Wang<senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "cache_helper.h"
#include "comm_define.h"

#define BLOCK_SIZE 8192

typedef struct { 
	CACHE_CTRL *cache_ctrl;
} Fixture;

Fixture fixture;

void case_setup()
{ 
    int ret;
	g_message("--enter fun %s", __func__);
	fixture.cache_ctrl = cache_new();
    g_assert(fixture.cache_ctrl!=NULL);
    ret = cache_init(fixture.cache_ctrl, BLOCK_SIZE, 1024, 1000, 80, 100);
    g_assert(ret == 0);
}

/*  base query  */
void test_cache_query_1()
{
	g_message("--enter fun %s", __func__);
	int ret = 0;
	char *_block_buf = NULL;
	_block_buf = (char *)g_malloc0(BLOCK_SIZE);
	sprintf(_block_buf, "hello cache mine");
	ret = cache_insert_block(fixture.cache_ctrl, 1, _block_buf);
    printf("ret is :%d\n", ret);
	g_assert(ret == 0);
	sprintf(_block_buf, "hello cache you");
	ret = cache_insert_block(fixture.cache_ctrl, 2, _block_buf);
    printf("ret is :%d\n", ret);
	g_assert(ret == 0);
	sprintf(_block_buf, "hello cache him");
	ret = cache_insert_block(fixture.cache_ctrl, 4, _block_buf);
	g_assert(ret == 0);
    int i = 0;
    for(i = 8; i < 200; i++) {
	    ret = cache_insert_block(fixture.cache_ctrl, i, _block_buf);
	    g_assert(ret == 0);
    }
    char *block_buf = g_malloc0(fixture.cache_ctrl->block_size);
    ret = cache_query_block(fixture.cache_ctrl, 1, block_buf);
    g_assert(ret == 0);
    ret = cache_query_block(fixture.cache_ctrl, 2, block_buf);
    g_assert(ret == 0);
    ret = cache_query_block(fixture.cache_ctrl, 3, block_buf);
    g_assert(ret != 0);
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
	g_test_add("/misc/cache_query_1", 
				Fixture, 
				NULL,
				case_setup, 
				test_cache_query_1, 
				case_teardown);
	
	return g_test_run();
}
