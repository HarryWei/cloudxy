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
static int test_printf(void* handle,char* buf,uint32_t start_no,uint32_t end_no){
    printf("%d,%d\n", start_no, end_no);
    return 0;
}
void case_setup()
{

	int ret = 0;
    CACHE_CTRL * cache_ctrl = cache_new();
    fixture.cache_ctrl = cache_ctrl;
    ret = cache_init(fixture.cache_ctrl,8192,1024,3,80,10);
    ret = cache_set_write_cb(fixture.cache_ctrl,test_printf,NULL);
    //block_t *block = (block_t*)g_trash_stack_pop(&fixture.cache_ctrl->block_cache);
    //char buf[]="xxxxxxxxxxxxxx";
    //memcpy(block->block,buf,strlen(buf));
    //block->block_no = 10;
    //g_queue_push_tail(fixture.cache_ctrl->dirty_block,(gpointer)block);
    //g_hash_table_insert(fixture.cache_ctrl->block_map,(gpointer)&block->block_no,(gpointer)block);
}

/*
 * case1 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

/*  write trigger wb*/
void test_case_cache_flush_1()
{
	int ret = 0;
    char *buf = g_malloc0(fixture.cache_ctrl->block_size);
    int i = 0;
    for(i=0;i<1024;i++){
	    ret = cache_insert_block(fixture.cache_ctrl,i,buf);
        g_assert(ret == 0);
    }
}

void test_case_cache_flush_2()
{
	int ret = 0;
    char *buf = g_malloc0(fixture.cache_ctrl->block_size*8);
    int i = 0;
    for(i=0;i<1024/8;i++){
	    ret = cache_insert_blocks(fixture.cache_ctrl,i*8,8,buf);
        g_assert(ret == 0);
    }
}

void test_case_cache_flush_3()
{
	int ret = 0;
    char *buf = g_malloc0(fixture.cache_ctrl->block_size*64);
    int i = 0;
    for(i=0;i<1024/64;i++){
	    ret = cache_insert_blocks(fixture.cache_ctrl,i*64,64,buf);
        g_assert(ret == 0);
    }
}

void test_case_cache_flush_4()
{
	int ret = 0;
    char *buf = g_malloc0(fixture.cache_ctrl->block_size*64);
    int i = 0;
    for(i=0;i<1024*64/64;i++){
	    ret = cache_insert_blocks(fixture.cache_ctrl,i*64,64,buf);
        g_assert(ret == 0);
    }
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

}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
#if 1
	g_test_add("/misc/cache_flush_work_1", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_1, 
				case_teardown);
#endif
#if 1
	g_test_add("/misc/cache_flush_work_2", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_2, 
				case_teardown);
#endif 
#if 1
    g_test_add("/misc/cache_flush_work_3", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_3, 
				case_teardown);
#endif
#if 1
    g_test_add("/misc/cache_flush_work_4", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_4, 
				case_teardown);
#endif

	return g_test_run();
}
