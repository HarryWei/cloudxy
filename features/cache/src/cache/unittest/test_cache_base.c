/*
 * Unit test for hlfs_take_snapshot();
 * cases:
 * case1: We opened hlfs in the mode readonly.
 * case2: ssname has been used.
 * case3: normal.
 * By Kelvin <kelvin.xupt@gmail.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "cache.h"
#include "comm_define.h"

typedef struct {
    CACHE_CTRL *cache_ctrl;
} Fixture;

/* 
 * case1 setup: Build the environment for hlfs_take_snapshot().
 * Include:
 * --Format HLFS;
 * --Write something to HLFS;
 * --init and open hlfs readonly;
 */
Fixture fixture;
void case_setup()
{
	int ret = 0;
}

/*
 * case1 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

void test_case_cache_new()
{
	int ret = 0;
    CACHE_CTRL * cache_ctrl = cache_new();
    fixture.cache_ctrl = cache_ctrl;
    g_assert(cache_ctrl!=NULL);
}

void test_case_cache_init()
{
	int ret = 0;
    ret = cache_init(fixture.cache_ctrl,8192,1024,100,80,1024);
    g_assert(ret==0);
}

/*
 * case1 teardown:
 * --close hlfs;
 * --deinit hlfs;
 * --free the structure case1_fixture;
 * --rm hlfs root directory;
 */
void case_teardown()
{
	//hlfs_close(case1_fixture.ctrl);
	//deinit_hlfs(case1_fixture.ctrl);
	//if (case1_fixture.uri != NULL)
	//	g_free(case1_fixture.uri);
	//system("rm -rf /tmp/testenv");
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/cache_new", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_new, 
				case_teardown);
	g_test_add("/misc/cache_init", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_init, 
				case_teardown);
	
	return g_test_run();
}
