/*
 *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 as published by
 *  the Free Software Foundation.
 */

#include "glib.h"
#include <stdlib.h>
#include <string.h>
#include "storage_helper.h"
#include "comm_define.h"
#include "hlfs_log.h"

static gchar *poolUri = NULL;
static gchar *fsname = NULL;
static gchar *uri = NULL;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	{"pool location", 'p', 0, G_OPTION_ARG_STRING, &poolUri, "filesystem storage pool uri", "POOLLOC"},
	{"filesystem name", 'u', 0, G_OPTION_ARG_STRING, &fsname, "filesystem name", "FSNAME"},
        {"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
        {NULL}
};

static void 
error_func(GOptionContext *context, GOptionGroup *group, \
		gpointer data, GError **error) 
{
	if (*error && (*error)->message) {
		gchar *progname = g_get_prgname();
		g_print("%s: %s\nTry '%s --help' for more information.\n", 
				progname, (*error)->message, progname);
		exit(EXIT_FAILURE);
	}
}

/*  mkfs.stat -p http://<poolpath>  -u name */
int main(int argc, char *argv[])
{
//	if (log4c_init()) {
//		g_message("log4c_init failed!");
//	}
	GError *error = NULL;
	GOptionContext *context;
	context = g_option_context_new("- hlfs stat");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_help_enabled(context, TRUE);
	g_option_group_set_error_hook(g_option_context_get_main_group(context),
			(GOptionErrorFunc)error_func);
	if (!g_option_context_parse(context, &argc, &argv, &error)) {
		g_message("option parsing failed: %s", error->message);
		exit(EXIT_FAILURE);
	}

	g_print("pool location is :%s\n",poolUri);
	g_print("fsname   is :%s\n",fsname);
	//g_print("block size   is :%d\n",block_size);
	//g_print("segment size   is :%d\n",seg_size);
	//seg_size = SEGMENT_SIZE;
        if(fsname == NULL){
   	   //g_print("get pool info ------ \n");
           uri = g_build_filename(poolUri,"dump",NULL); 
        }else{
           //g_print("get fs info   ------ \n");
           exit(0);
        }        
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		g_message("can not get storage handler for uri:%s", poolUri);
		g_option_context_free(context);
		return -1;
	}
        uint64_t capacity,used;
        if (0 != storage->bs_get_capacity(storage,&capacity)){
     	   g_printf(" can not get total Capacity ");
           return -1;
        }
        if (0 != storage->bs_get_used(storage,&used)){
     	   g_printf(" can not get used Capacity ");
           return -1;
        }
     	g_printf("Pool Total Capacity:%lld,Used:%lld\n",capacity,used);
        g_free(uri);
	g_option_context_free(context);
	deinit_storage_handler(storage);
//	if (log4c_fini()) {
//		g_message("log4c_fini failed!");
//	}
	return 0;
}
