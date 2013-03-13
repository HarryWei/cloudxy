#include <string.h>
#include <stdint.h>
#include "api/hlfs.h"
#include "hlfs_log.h"
#include "glib.h"
#include "dentry.h"

static gchar *uri = NULL;
static gchar *f_path = NULL;
static gint request_size = 0;
static gint total_size = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri/conf", "FSLOC"},
	    {"filepath",'f', 0, G_OPTION_ARG_STRING,   &f_path, "file path", "FPATH"},
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
	g_option_context_free(context);
    g_print("TEST: uri is %s, f_path is %s, request size is %d, total size is %d\n", 
												uri, f_path, request_size, total_size);
    char *content = (char*)g_malloc0(request_size);
	g_message("before init hlfs...");
    HLFS_CTRL * ctrl = init_hlfs(uri);
	g_message("after init hlfs...");
    g_assert(ctrl != NULL);
    uint64_t ret = 0;
	ret = hlfs_create(ctrl, f_path, 1);
	g_assert(ret == 0);
	g_message("ctrl->rw_inode_flag: %d", ctrl->rw_inode_flag);
#if 1
    ret = hlfs_fopen(ctrl, f_path, 1);
	g_message("ctrl->rw_inode_flag: %d", ctrl->rw_inode_flag);
    g_assert(ret == 0);
    g_print("TEST  hlfs fopen over \n");
#endif
	g_print("test hlfs fwrite\n");
	sleep(2);
    int offset = 0;
    while(offset < total_size){
        ret = hlfs_fwrite(ctrl, f_path, content, request_size, offset);
		g_message("request_size: %d, ret size: %d", request_size, ret);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
    g_print("TEST  hlfs write over \n");
	g_print("test hlfs read\n");
	sleep(2);
    offset = 0;
    while(offset < total_size){
        ret = hlfs_fread(ctrl,f_path,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
#if 0
	g_print("again ------------------------\n");
    offset = 0;
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
#endif
    g_free(content);
	ret = hlfs_fclose(ctrl, f_path);
    deinit_hlfs(ctrl);
    g_print("TEST  hlfs test over \n");
    return 0;
}
#endif
