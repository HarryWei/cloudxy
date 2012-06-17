/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "storage_helper.h"
#include "seg_clean.h"
#include "glib.h"
#include "hlfs_log.h"
#include "api/hlfs.h"
#include "comm_define.h"
 
static gchar *uri = NULL;
//static gchar *fsname = NULL;
static gint   start_segno = 0;
static gint   end_segno = 0;
static gint   copy_waterlevel = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem uri", 'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
    	{"start segno to be move",    's', 0, G_OPTION_ARG_INT,      &start_segno,    "start segno to be moved", "START SEGNO"},
    	{"end   segno to be move",    'e', 0, G_OPTION_ARG_INT,      &end_segno,      "start segno to be moved", "END SEGNO"},
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
    g_message("start segno:%d,end segno:%d",start_segno,end_segno);
    g_message("copy_waterlevel is: %d",copy_waterlevel);

    g_option_context_free(context);
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    g_message("ctrl init over");
    int ret = 0;
    ret = hlfs_open(ctrl,1);
    g_message("ctrl open over");
    g_assert(ret == 0);
    if(0!=ctrl->storage->bs_file_is_exist(ctrl->storage,SEGMENTS_USAGE_FILE)){
       g_message("seg usage file not exit");
       goto OUT;
    }
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    GList* seg_usage_list = NULL;
    ret = load_all_seg_usage(ctrl->storage,SEGMENTS_USAGE_FILE,seg_usage_hashtable);
    g_assert(ret == 0);
    ret = sort_all_seg_usage(seg_usage_hashtable,&seg_usage_list);
    g_assert(ret == 0);
    int i;
    for(i=start_segno;i<=end_segno;i++){
        SEG_USAGE_T *seg_usage = g_hash_table_lookup(seg_usage_hashtable,GINT_TO_POINTER((uint32_t)i));
        char segfile[128];
        build_segfile_name(i, segfile);
        if(seg_usage != NULL){
            g_message("seg no:%d ...",seg_usage->segno);
            if (seg_usage->alive_block_num == 0){
                g_message("seg no:%d no alive block now,delete it",i);
                if(0 == ctrl->storage->bs_file_is_exist(ctrl->storage,segfile)){
                    ret = ctrl->storage->bs_file_delete(ctrl->storage,segfile);	  
                    g_assert(ret == 0);
                }else{
                    g_message("seg no:%d has delete",i);
                }
                continue;
            }			
            if(0 == strcmp(seg_usage->up_sname,EMPTY_UP_SNAPSHOT)){
                g_message("seg no:%d is maybe need to migrate",seg_usage->segno);
                if (seg_usage->alive_block_num > copy_waterlevel){
                    g_message("seg no:%d is need to migrate",seg_usage->segno);
                    ret = migrate_alive_blocks(ctrl,seg_usage);
                    g_assert(ret == 0);
                    seg_usage->alive_block_num =0;
                    seg_usage->timestamp = get_current_time();
                    memset(seg_usage->bitmap,0,(seg_usage->log_num-1)/sizeof(gint)+1);
                    ret = dump_seg_usage(ctrl->storage,SEGMENTS_USAGE_FILE,seg_usage);
                    g_free(seg_usage->bitmap);
                    g_free(seg_usage);
                    ret = ctrl->storage->bs_file_delete(ctrl->storage,segfile);	  
                }
            }
        }else{
            g_message("seg no:%d has not yet do seg usage calc",i); 
        }
    }
OUT:
    ret = hlfs_close(ctrl);
    g_assert(ret == 0);
    ret = deinit_hlfs(ctrl);
    g_assert(ret ==0);
    return 0;
}
