#include <glib.h>
#include <stdlib.h>
#include "seg_clean_helper.h"
#include "seg_clean.h"
#include "comm_define.h"
#include "api/hlfs.h"

typedef struct {
} Fixture;

/* 
 * case1 setup: Build the environment for hlfs_take_snapshot().
 * Include:
 * --Format HLFS;
 * --Write something to HLFS;
 * --init and open hlfs readonly;
 */
Fixture fixture;

static void build_segments_with_cleanall(){
    char * uri = "local:///tmp/testenv/testfs";
    struct hlfs_ctrl * hctrl = init_hlfs(uri);
    g_assert(NULL != hctrl);
    int ret = 0;
    ret = hlfs_open(hctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
    int i = 0;
    int offset = 0;
    int BATCHSIZE = 8192;
    char content[BATCHSIZE];
    for(i=0;i<8192;i++){
        hlfs_write(hctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
    }
    for(i=0;i<8192;i++){
        hlfs_write(hctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
    }
    hlfs_close(hctrl);
	deinit_hlfs(hctrl);
}


static void build_segments_without_snapshot(){
    char * uri = "local:///tmp/testenv/testfs";
    struct hlfs_ctrl * hctrl = init_hlfs(uri);
    g_assert(NULL != hctrl);
    int ret = 0;
    ret = hlfs_open(hctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
    int i = 0;
    int offset = 0;
    int BATCHSIZE = 8192;
    char content[BATCHSIZE];
    for(i=0;i<8192;i++){
        hlfs_write(hctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
    }
    hlfs_close(hctrl);
	deinit_hlfs(hctrl);
}

static void build_segments_with_snapshot(){
    char * uri = "local:///tmp/testenv/testfs";
    struct hlfs_ctrl * hctrl = init_hlfs(uri);
    g_assert(NULL != hctrl);
    int ret = 0;
    ret = hlfs_open(hctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
    int i = 0;
    int offset = 0;
    int BATCHSIZE = 8192;
    char content[BATCHSIZE];
    for(i=0;i<8192*2;i++){
        hlfs_write(hctrl,content,BATCHSIZE,offset);
        hlfs_write(hctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
        if(offset == 8192*8192/2){
           ret = hlfs_take_snapshot(hctrl,"test_snapshot1");  
           g_assert(ret == 0);
           g_print("take snapshot 1\n");
        }
        if(offset == 8192*8192){
           ret = hlfs_take_snapshot(hctrl,"test_snapshot2");  
           g_assert(ret == 0);
           g_print("take snapshot 2\n");
        }
        if(offset == 8192*8192 + 8192*1){
           ret = hlfs_take_snapshot(hctrl,"test_snapshot3");  
           g_assert(ret == 0);
           g_print("take snapshot 3\n");
        }
    }
    hlfs_close(hctrl);
	deinit_hlfs(hctrl);
}

static void env_reset(){
	system("rm -rf /tmp/testenv");
	system("mkdir /tmp/testenv -p");
	system("cd ../../../ && ./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024");
	system("cd -");
}

void case_setup()
{
}

/*
 * case1 test:
 */

void test_hlfs_seg_usage_calc_1()
{
    env_reset();
    build_segments_without_snapshot();
    //system("cd ../../../ && ./output/bin/segcalc.hlfs 
}

void test_hlfs_seg_usage_calc_2()
{
    env_reset();
    build_segments_with_snapshot();
}

void test_hlfs_seg_usage_calc_3()
{
    env_reset();
    build_segments_with_cleanall();
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
	//deinit_hlfs(hctrl);
    //system("rm -rf /tmp/testenv -rf");
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
#if 0
	g_test_add("/misc/test_hlfs_seg_usage_calc_1", 
				Fixture, 
				NULL,
				case_setup, 
				test_hlfs_seg_usage_calc_1, 
				case_teardown);
#endif
#if 0
	g_test_add("/misc/test_hlfs_seg_usage_calc_2", 
				Fixture, 
				NULL,
				case_setup, 
				test_hlfs_seg_usage_calc_2, 
				case_teardown);
#endif 
#if 1
    g_test_add("/misc/test_hlfs_seg_usage_calc_3", 
				Fixture, 
				NULL,
				case_setup, 
				test_hlfs_seg_usage_calc_3, 
				case_teardown);
#endif
#if 0
    g_test_add("/misc/cache_flush_work_4", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_4, 
				case_teardown);
#endif

	return g_test_run();
}

