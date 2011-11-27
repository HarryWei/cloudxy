#include "glib.h"
#include "api/hlfs.h"
#include <string.h>
#include "hlfs_log.h"

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

    g_print("TEST : uri%s request size:%d,total size:%d\n",uri,request_size,total_size);
    char *content = (char*)g_malloc0(request_size);
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    uint64_t ret = 0;
    ret = hlfs_open(ctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
    int offset = 0;
    while(offset < total_size){
        ret = hlfs_write(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
    g_print("TEST  hlfs write over \n");
    offset = 0;
    while(offset < total_size){
        ret = hlfs_read(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
    ret = hlfs_close(ctrl);
    deinit_hlfs(ctrl);
    return 0;
}
