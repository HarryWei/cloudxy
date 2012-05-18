#include <glib.h>
#include <stdlib.h>
#include "seg_clean_helper.h"
#include "seg_clean.h"
#include "comm_define.h"
#include "api/hlfs.h"

typedef struct {
     struct hlfs_ctrl  *hctrl;
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
#if 1
	system("rm -rf /tmp/testenv");
	system("mkdir /tmp/testenv -p");
	system("cd ../../../ && ./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024");
	system("cd -");
    char * uri = "local:///tmp/testenv/testfs";
    fixture.hctrl = init_hlfs(uri);
    g_assert(NULL != fixture.hctrl);
    int ret = 0;
    ret = hlfs_open(fixture.hctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
    int i = 0;
    int offset = 0;
    int BATCHSIZE = 8192;
    char content[BATCHSIZE];
    for(i=0;i<8192*3;i++){
        hlfs_write(fixture.hctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
        if(offset == 8192*8192){
           ret = hlfs_take_snapshot(fixture.hctrl,"test_snapshot1");  
           g_assert(ret == 0);
           g_print("take snapshot 1\n");
        }

        if(offset == 8192*8192*2){
           ret = hlfs_take_snapshot(fixture.hctrl,"test_snapshot2");  
           g_assert(ret == 0);
           g_print("take snapshot 2\n");
        }

        if(offset == 8192*8192*2 + 8192*1){
           ret = hlfs_take_snapshot(fixture.hctrl,"test_snapshot3");  
           g_assert(ret == 0);
           g_print("take snapshot 3\n");
        }
    }
    hlfs_close(fixture.hctrl);
	deinit_hlfs(fixture.hctrl);
#endif 
}

/*
 * case1 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

/*  write trigger wb*/
void test_get_refer_inode()
{
    char * uri = "local:///tmp/testenv/testfs";
    struct back_storage *storage = init_storage_handler(uri);
    int ret = 0;
    uint32_t segment_size = 0;
    uint32_t block_size = 0;
    uint64_t max_fs_size = 0;
    ret = read_fs_meta(storage,&segment_size, &block_size,&max_fs_size);
    g_assert(ret == 0);
    GHashTable   * ss_hashtable = g_hash_table_new_full(g_str_hash,g_str_equal,NULL,NULL);
    ret = load_all_snapshot(storage,"snapshot.txt",ss_hashtable);
    printf("snapshot loaded\n"); 
    g_assert(ret == 0);
    GList* ss_list = NULL;
    ret = sort_all_snapshot(ss_hashtable,&ss_list);
    printf("snapshot sorted\n"); 
    g_assert(ss_list !=NULL);
    g_assert(ret == 0);
    int i;
    for(i=0;i<9;i++){
        struct inode * inode=NULL;
        char *up_sname;
        ret = get_refer_inode_between_snapshots(storage,i,ss_list,&inode,&up_sname);
        printf("segno :%d ret:%d\n",i,ret);
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
	//deinit_hlfs(fixture.hctrl);
    //system("rm -rf /tmp/testenv -rf");
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
#if 1
	g_test_add("/misc/test_get_refer_inode", 
				Fixture, 
				NULL,
				case_setup, 
				test_get_refer_inode, 
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

