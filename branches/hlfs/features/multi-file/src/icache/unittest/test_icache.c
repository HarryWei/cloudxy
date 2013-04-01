
#include "glib.h"
#include <stdlib.h>
#include "cache.h"
#include "cache_helper.h"
#include "comm_define.h"
#include "api/hlfs.h"

#define IBLOCK_SIZE 8192
typedef struct { 
	ICACHE_CTRL *icache_ctrl;
} Fixture;


Fixture fixture;
void case_setup()
{ 
    int ret;
    g_message("--enter fun %s", __func__);
    fixture.icache_ctrl = icache_new();
    g_assert(fixture.icache_ctrl!=NULL);
    ret = icache_init(fixture.icache_ctrl, IBLOCK_SIZE, 1024, 80, 128);
    g_assert(ret == 0);
}
/*  base insert  */
void test_icache_insert_1()
{
    g_message("--enter fun %s", __func__);
    int ret = 0;
    char *_iblock_buf = NULL;
    _iblock_buf = (char *)g_malloc0(IBLOCK_SIZE);
    sprintf(_iblock_buf, "hello icache mine");
    ret = icache_insert_iblock(fixture.icache_ctrl, 1, _iblock_buf);
    printf("ret is :%d\n", ret);
    g_assert(ret == 0);
    sprintf(_iblock_buf, "hello cache you");
    ret = icache_insert_iblock(fixture.icache_ctrl, 2, _iblock_buf);
    printf("ret is :%d\n", ret);
    g_assert(ret == 0);
    sprintf(_iblock_buf, "hello cache him");
    ret = icache_insert_iblock(fixture.icache_ctrl, 4, _iblock_buf);
    g_assert(ret == 0);
}

void test_icache_insert_2()
{
    int ret;
    int i;
	char *_iblock_buf = NULL;
	_iblock_buf = (char *)g_malloc0(IBLOCK_SIZE);
	for(i=0;i<2048;i++){
		sprintf(_iblock_buf, "hello icache mine");
	       ret = icache_insert_iblock(fixture.icache_ctrl, i, _iblock_buf);
		g_assert(ret == 0);
	}	
}


void test_icache_query_1()
{
    int ret =0;
    int i;
    char *_iblock_buf = NULL;
    _iblock_buf = (char *)g_malloc0(IBLOCK_SIZE);
    for(i=0;i<1024;i++){
        //printf("ibno:%d\n",i);
        //printf("size:%d\n",g_queue_get_length(fixture.icache_ctrl->iblock_lru));
        sprintf(_iblock_buf, "hello icache mine");
        ret = icache_insert_iblock(fixture.icache_ctrl, i, _iblock_buf);
        g_assert(ret == 0);
    }	
    for(i=0;i<128;i++){
        ret = icache_query_iblock(fixture.icache_ctrl, i,_iblock_buf);
        g_assert(ret != 0);
        printf("iblock :%s\n",_iblock_buf);
    }
    for(i=128;i<1024;i++){
        ret = icache_query_iblock(fixture.icache_ctrl, i,_iblock_buf);
        g_assert(ret == 0);
        printf("iblock :%s\n",_iblock_buf);
    }

}

void test_icache_query_2()
{
    int ret = 0;
       int i;
	char *_iblock_buf = NULL;
	_iblock_buf = (char *)g_malloc0(IBLOCK_SIZE);
	for(i=0;i<1024;i++){
		sprintf(_iblock_buf, "hello icache mine");
	    ret = icache_insert_iblock(fixture.icache_ctrl, i, _iblock_buf);
		g_assert(ret == 0);
	}	
	for(i=128;i<512;i++){
		sprintf(_iblock_buf, "hello icache mine");
	    ret = icache_query_iblock(fixture.icache_ctrl, i, _iblock_buf);
		g_assert(ret == 0);
	}
	for(i=1024;i<2048;i++){
		sprintf(_iblock_buf, "hello icache mine");
	    ret = icache_insert_iblock(fixture.icache_ctrl, i, _iblock_buf);
		g_assert(ret == 0);
	}	
}


void case_teardown()
{
	if (0 > icache_destroy(fixture.icache_ctrl)) {
		g_message("destroy icache error");
		return;
	}	
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/icache_insert_1", 
				Fixture, 
				NULL,
				case_setup, 
				test_icache_insert_1, 
				case_teardown);
	g_test_add("/misc/icache_insert_2", 
				Fixture, 
				NULL,
				case_setup, 
				test_icache_insert_2, 
				case_teardown);
	g_test_add("/misc/icache_query_1", 
				Fixture, 
				NULL,
				case_setup, 
				test_icache_query_1, 
				case_teardown);
	g_test_add("/misc/icache_query_2", 
				Fixture, 
				NULL,
				case_setup, 
				test_icache_query_2, 
				case_teardown);
	
	return g_test_run();
}
