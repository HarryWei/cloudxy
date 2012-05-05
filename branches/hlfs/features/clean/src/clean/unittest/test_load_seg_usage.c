#include <glib.h>
#include <stdlib.h>
#include "seg_clean_helper.h"
#include "seg_clean.h"

#include "comm_define.h"

typedef struct {
     struct back_storage *storage;
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
 	system("rm -rf /tmp/testenv");
	system("mkdir /tmp/testenv/testfs -p");
	//system("cd ../../../ && ./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024 ");
	//system("cd -");
	char * uri = "local:///tmp/testenv/testfs";
    fixture.storage = init_storage_handler(uri);
    g_assert(NULL != fixture.storage);
}

/*
 * case1 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

/*  write trigger wb*/
void test_load_seg_usage()
{
    int ret = 0;
    int i;
    for(i=0;i<100;i++){
        SEG_USAGE_T seg_usage;
        seg_usage.segno = 10+i;
        sprintf(seg_usage.up_sname,"%s%d","test_snapshot_",i);
        seg_usage.block_num = 64+i;
        seg_usage.inode_addr = 10101010+i;
        seg_usage.log_num = 8+i;
        seg_usage.timestamp = get_current_time();
        seg_usage.alive_block_num = 10+i;
        seg_usage.bitmap = g_malloc0((seg_usage.log_num-1)/sizeof(gint)+1);
        int i,idx;
        i = 0;idx = i/sizeof(gint);
        seg_usage.bitmap[idx] |= 1<<0%sizeof(gint);
        i = 2;idx = i/sizeof(gint);
        seg_usage.bitmap[idx] |= 1<<2%sizeof(gint);
        i = 7;idx = i/sizeof(gint);
        i = 0;idx = i/sizeof(gint);
        seg_usage.bitmap[idx] |= 1<<7%sizeof(gint);
        char textbuf[4096];
        memset(textbuf,4096,0);
        ret = seg_usage2text(&seg_usage,textbuf);
        g_assert(ret > 0);
        printf("textbuf is =:%s\n",textbuf);
        int ret = file_append_contents(fixture.storage,"SEG_USAGE.TXT",textbuf,strlen(textbuf));
        printf("textbuf len:%d,ret:%d\n",strlen(textbuf),ret);
        g_assert(ret == 0);
    }

    GHashTable   * seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);
    ret = load_all_seg_usage(fixture.storage,"SEG_USAGE.TXT",seg_usage_hashtable);
    g_assert(ret == 0);
    GList* seg_list;
    ret = sort_all_seg_usage(seg_usage_hashtable,&seg_list);
    g_assert(ret == 0);
    g_assert(100 == g_list_length(seg_list));
    int j = 0;
    for(j=0;j<100;j++){
         SEG_USAGE_T * seg_usage = g_list_nth_data(seg_list,j);  
         printf("seg no:%d\n",seg_usage->segno);
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
	deinit_storage_handler(fixture.storage);
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
#if 1
	g_test_add("/misc/test_load_seg_usage", 
				Fixture, 
				NULL,
				case_setup, 
				test_load_seg_usage, 
				case_teardown);
#endif
#if 0
	g_test_add("/misc/cache_flush_work_2", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_2, 
				case_teardown);
#endif 
#if 0
    g_test_add("/misc/cache_flush_work_3", 
				Fixture, 
				NULL,
				case_setup, 
				test_case_cache_flush_3, 
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

