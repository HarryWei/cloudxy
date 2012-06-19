#include <string.h>
#include <stdint.h>
#include "api/hlfs.h"
#include "seg_clean.h"
#include "seg_clean_helper.h"
#include "hlfs_log.h"
#include "glib.h"

//static gchar *uri = NULL;
//static gint request_size = 0;
//static gint total_size = 0;
//static gboolean verbose = FALSE;
//static GOptionEntry entries[] = {
//	    {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri/conf", "FSLOC"},
//    	{"filesystem request size", 'r', 0, G_OPTION_ARG_INT, &request_size, "test request size", "REQUESTSIZE"},
//    	{"filesystem tatal size", 'a', 0, G_OPTION_ARG_INT, &total_size, "test total request size", "TOTALSIZE"},
//    	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
//    	{NULL}
//};

//static void 
//error_func(GOptionContext *context, GOptionGroup *group, 
//				gpointer data, GError **error) 
//{
//    if (*error && (*error)->message) {
//        gchar *progname = g_get_prgname();
//        g_print("%s: %s\nTry '%s --help' for more information.\n", 
//				progname, (*error)->message, progname);
//        exit(EXIT_FAILURE);
//    }
//}
#if 1
/*
 * ./test -u uri -r request_size -a total_size
 * uri: /tmp/testenv/testfs
 * request_size: 4096bytes
 * total_size: 409600bytes
 */
int main(int argc, char *argv[]){
    if (log4c_init()) {
        g_message("log4c_init error!");
    }
    //GError *error = NULL;
    //GOptionContext *context;
    //context = g_option_context_new("- hlfs test -");
    //g_option_context_add_main_entries(context, entries, NULL);
    //g_option_context_set_help_enabled(context, TRUE);
    //g_option_group_set_error_hook(g_option_context_get_main_group(context),
    //        (GOptionErrorFunc)error_func);
    //if (!g_option_context_parse(context, &argc, &argv, &error)) {
    //    g_message("option parsing failed: %s", error->message);
    //    exit(EXIT_FAILURE);
    //}
	//g_option_context_free(context);

	system("rm -rf /tmp/testenv");
	system("mkdir /tmp/testenv -p");
	system("cd ../ && ./output/bin/mkfs.hlfs -u local:///tmp/testenv/testfs -b 8192 -s 67108864 -m 1024");
	system("cd -");
	g_print("TEST  mkfs testfs over !!!!!!!!!!!!!!!!!!!\n");
    //g_print("TEST:uri is %s, request size is %d, total size is %d\n", uri, request_size, total_size);

    char *uri = "local:///tmp/testenv/testfs";
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    uint64_t ret = 0;
    ret = hlfs_open(ctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over !!!!!!!!!!!!!!!!!!!!!\n");
	g_print("TEST  make data and snapshot\n");
    int i;
    uint32_t BATCHSIZE = 8192;
    char *content = (char*)g_malloc0(BATCHSIZE);
    uint32_t offset = 0;
    for(i=0;i<8192*2;i++){
        hlfs_write(ctrl,content,BATCHSIZE,offset);
        hlfs_write(ctrl,content,BATCHSIZE,offset);
        offset = offset + BATCHSIZE;
        if(offset == 8192*8192/2){
           ret = hlfs_take_snapshot(ctrl,"test_snapshot1");  
           g_assert(ret == 0);
           g_print("take snapshot 1\n");
        }
        if(offset == 8192*8192*1){
           ret = hlfs_take_snapshot(ctrl,"test_snapshot2");  
           g_assert(ret == 0);
           g_print("take snapshot 2\n");
        }
        if(offset == 8192*8192*1 + 8192*1){
           ret = hlfs_take_snapshot(ctrl,"test_snapshot3");  
           g_assert(ret == 0);
           g_print("take snapshot 3\n");
        }
    }
    g_free(content);
	g_print("TEST  make data and snapshot over !!!!!!!!!!!!!!!!\n");
	g_print("TEST  make seg usage calc \n");
    GHashTable   * ss_hashtable = g_hash_table_new_full(g_str_hash,g_str_equal,NULL,NULL);
    ret = load_all_snapshot(ctrl->storage,"snapshot.txt",ss_hashtable);
    printf("snapshot loaded\n"); 
    g_assert(ret == 0);
    GList* ss_list = NULL;
    ret = sort_all_snapshot(ss_hashtable,&ss_list);
    printf("snapshot sorted\n"); 
    g_assert(ss_list !=NULL);
    g_assert(ret == 0);
    int max_seg = ctrl->last_segno;
    for(i=0;i<=max_seg;i++){
        struct inode * inode=NULL;
        char *up_sname;
        ret = get_refer_inode_between_snapshots(ctrl->storage,i,ss_list,&inode,&up_sname);
        printf("segno :%d ret:%d\n",i,ret);
        if(ret == 0){
           printf("seg is in snapshots\n");
           SEG_USAGE_T seg_usage;
           memset(&seg_usage,0,sizeof(SEG_USAGE_T));
           strncpy(seg_usage.up_sname,up_sname,strlen(up_sname));
           ret = seg_usage_calc(ctrl->storage,ctrl->sb.block_size,i,inode,&seg_usage);
           printf("up sname is:%s\n",seg_usage.up_sname);
           g_assert(ret ==0);
           char textbuf[8192];
           memset(textbuf,0,8192);
           ret = seg_usage2text(&seg_usage,textbuf);
           g_assert(ret > 0);
           printf("textbuf is :%s\n",textbuf);
           ret = dump_seg_usage(ctrl->storage,"segments_usage.txt",&seg_usage);
           g_assert(ret == 0);
           printf("dump seg usage over !");
        }
        if(ret == 1){
           printf("seg is on snapshot,do nothing\n");
        }
        if(ret == 2){
           printf("seg is above snapshot,maybe need migrate\n");
           SEG_USAGE_T seg_usage;
           memset(&seg_usage,0,sizeof(SEG_USAGE_T));
           strncpy(seg_usage.up_sname,"_____",strlen("_____"));
           printf("up sname is:%s\n",seg_usage.up_sname);
           ret = seg_usage_calc(ctrl->storage,ctrl->sb.block_size,i,&ctrl->inode,&seg_usage);
           g_assert(ret ==0);
           char textbuf[8192];
           memset(textbuf,0,8192);
           ret = seg_usage2text(&seg_usage,textbuf);
           g_assert(ret > 0);
           printf("textbuf is :%s\n",textbuf);
           ret = dump_seg_usage(ctrl->storage,"segments_usage.txt",&seg_usage);
           g_assert(ret == 0);
        }
    }
	g_print("TEST  make seg usage calc over !!!!!!!!!!!!!!!!\n");
    
	g_print("TEST  wait to seg clean >>>>\n");
    sleep(10000);

	ret = hlfs_close(ctrl);
    deinit_hlfs(ctrl);
    return 0;
}
#endif
