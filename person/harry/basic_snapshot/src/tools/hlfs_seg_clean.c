#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "storage_helper.h"
#include "segment_cleaner.h"
#include "glib.h"
#include "clean_route.h"
#include "hlfs_log.h"
#include "api/hlfs.h"
#include "comm_define.h"
 
static gchar *uri = NULL;
//static gchar *fsname = NULL;
static gint   segno = 0;
static gint   copy_waterlevel = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem uri", 'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
 //   	{"filesystme name",     'f', 0, G_OPTION_ARG_FILENAME, &fsname,   "filesystem name", "NAME"},
    	{"segno to be move",    's', 0, G_OPTION_ARG_INT,      &segno,    "segno to be moved", "SEGNO"},
    	{"copy waterlevel ",    'w', 0, G_OPTION_ARG_INT,      &copy_waterlevel, "copy waterlevel", "WATERLEVEL"},
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

/*  seg.clean -u uri -s segno -w copy_water -v verbose */
int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- segment clearn...");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }

    g_message("uri is :%s",uri);
    //g_message("fsname   is :%s",fsname);
    g_message("segno is :%d",segno);
    g_message("copy_waterlevel is: %d",copy_waterlevel);

    g_option_context_free(context);
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    g_message("ctrl init over");
    int ret = 0;
    ret = hlfs_open(ctrl,1);
    g_message("ctrl open over");
    g_assert(ret == 0);
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    ret = load_all_segment_usage(ctrl->storage,SEGMENTS_USAGE_FILE,SEGMENTS_DEL_FILE,seg_usage_hashtable);
    g_assert(ret == 0 );
    g_message("to load segment usage for seg:%d",segno);
    if(segno == -1){
        g_message("clean all segno");
        GList * seg_usage_list = g_hash_table_get_values(seg_usage_hashtable);
        if(g_list_length(seg_usage_list) == 0){
            g_message("no seg usage");
        }
        int i;
        for(i=0;i<g_list_length(seg_usage_list);i++){
            gpointer tmp = g_list_nth_data (seg_usage_list,i);
            struct segment_usage *seg_usage = (struct segment_usage*)tmp;
            if(seg_usage->alive_blocks > copy_waterlevel){
                continue;
            }
            ret = rewrite_alive_blocks_from_seg(ctrl,seg_usage);
            g_assert(ret != -1);
            dump_segment_delmark(ctrl->storage,SEGMENTS_DEL_FILE,seg_usage->segno);
            //char * segfile = build_segfile_name(seg_usage->segno);
	        const char segfile[SEGMENT_FILE_NAME_MAX];
	        build_segfile_name(seg_usage->segno, segfile);
            ctrl->storage->bs_file_delete(ctrl->storage,segfile);
            g_free(seg_usage->bitmap);
        }
        g_hash_table_destroy(seg_usage_hashtable);
    }else{
        struct segment_usage *seg_usage;
        gpointer tmp = g_hash_table_lookup(seg_usage_hashtable,GINT_TO_POINTER(segno));
        if(tmp == NULL){
           g_message("no such segno:%u in segment usage file for clean",segno);
           return -1;
        }
        seg_usage = (struct segment_usage*)tmp;
        ret = rewrite_alive_blocks_from_seg(ctrl,seg_usage);
        g_assert(ret != -1);
        dump_segment_delmark(ctrl->storage,SEGMENTS_DEL_FILE,seg_usage->segno);
        g_free(seg_usage->bitmap);
    }

    ret = hlfs_close(ctrl);
    g_assert(ret == 0);
    ret = deinit_hlfs(ctrl);
    g_assert(ret ==0);
    return 0;
}
