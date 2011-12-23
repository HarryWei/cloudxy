#include <string.h>
#include <stdint.h>
#include "api/hlfs.h"
#include "hlfs_log.h"
#include "glib.h"

static uint64_t get_current_time(void)
{
	HLOG_DEBUG("enter func %s", __func__);
	struct timeval t;

	if (gettimeofday(&t, NULL)) {
		HLOG_ERROR("Get current time error!");
		return -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return (uint64_t) t.tv_sec * 1000 + t.tv_usec / 1000;	
}

static gchar *uri = NULL;
static gint request_size = 0;
static gint total_size = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
    	{"filesystem request size", 'r', 0, G_OPTION_ARG_INT, &request_size, "test request size", "REQUESTSIZE"},
    	{"filesystem tatal size", 'a', 0, G_OPTION_ARG_INT, &total_size, "test total request size", "TOTALSIZE"},
    	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
    	{NULL}
};

static void 
error_func(GOptionContext *context, GOptionGroup *group, 
				gpointer data, GError **error) 
{
    if (*error && (*error)->message) {
        gchar *progname = g_get_prgname();
        g_print("%s: %s\nTry '%s --help' for more information.\n", 
				progname, (*error)->message, progname);
        exit(EXIT_FAILURE);
    }
}

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
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- hlfs test -");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            (GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }

    g_print("TEST: uri is %s, request size is %d, total size is %d\n", uri, request_size, total_size);
    char *content = (char*)g_malloc0(request_size);
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    uint64_t ret = 0;
    ret = hlfs_open(ctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
	g_print("test hlfs write\n");
	sleep(2);
    int offset = 0;
    while(offset < total_size){
        ret = hlfs_write(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
    g_print("TEST  hlfs write over \n");
	g_print("test hlfs read\n");
	sleep(2);
    offset = 0;
    while(offset < total_size){
        ret = hlfs_read(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
	g_print("test hlfs_find_inode_before_time\n");
	uint64_t time = get_current_time();
	g_print("time is %llu\n", time);
//	time = time - 3000;
	g_print("change it smaller: %llu\n", time);
	uint64_t inode_addr = 0;
	if (-1 == hlfs_find_inode_before_time(uri, time, &inode_addr)) {
		g_message("find inode before time error!");
		exit(-1);
	}
	g_print("inode_addr is %llu\n", inode_addr);
#if 0
	// test take snapshot
	ret = hlfs_take_snapshot(ctrl, "jiawei1");
	if (0 > ret) {
		g_message("take snapshot error!");
		exit(-1);
	}
	ret = hlfs_take_snapshot(ctrl, "jiawei2");
	if (0 > ret) {
		g_message("take snapshot error!");
		exit(-1);
	}
	ret = hlfs_take_snapshot(ctrl, "jiawei3");
	if (0 > ret) {
		g_message("take snapshot error!");
		exit(-1);
	}
	ret = hlfs_take_snapshot(ctrl, "jiawei4");
	if (0 > ret) {
		g_message("take snapshot error!");
		exit(-1);
	}
	// test rm snapshot
	ret = hlfs_rm_snapshot(uri, "jiawei1");
	if (0 > ret) {
		g_message("take snapshot error!");
		exit(-1);
	}
	char *lists = NULL;
	ret = hlfs_list_all_snapshots(uri, &lists);
	if (0 > ret) {
		g_message("list snapshots error!");
		exit(-1);
	}
	uint64_t inode_addr = 0;
	ret = hlfs_find_inode_by_name(uri, "jiawei3", &inode_addr);
	if (0 > ret) {
		g_message("find inode by sname error!");
		exit(-1);
	}
	g_message("inode addr is %llu", inode_addr);
#endif
    ret = hlfs_close(ctrl);
    deinit_hlfs(ctrl);
    return 0;
}
