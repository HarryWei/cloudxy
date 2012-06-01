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
#include "comm_define.h"
#include "hlfs_log.h"
#include "api/hlfs.h"
#include "hlfs_log.h"

static gchar *uri = NULL;
static gchar *snapshot = NULL;

static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	 {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
      	 {"snapshot", 's', 0, G_OPTION_ARG_STRING, &snapshot, "snapshot name", "SNAME"},
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

/*  mkfs.lhfs -l local://<path> -f name */
/*  mkfs.lhfs -l http://<path>  -s name */
int main(int argc, char *argv[])
{
	if (log4c_init()) {
		g_message("log4c_init failed!");
	}
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- mkfs hlfs");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }

    g_print("location is :%s\n",uri);
    g_print("snapshot   is :%s\n",snapshot);
    //g_print("block size   is :%d\n",block_size);
    //g_print("segment size   is :%d\n",seg_size);
    g_option_context_free(context);
    HLFS_CTRL *hctrl = init_hlfs(uri);
    g_assert(hctrl != NULL);
    if(0 != hlfs_open(hctrl,1)){
	 g_message("can not hctrl:%s",uri);
	 return -1;
    }		
    if(0 !=hlfs_take_snapshot(hctrl, snapshot) ){
	 g_message("cant not take snapshot:%s",snapshot);
	 return -1;
    }
    hlfs_close(hctrl);
    deinit_hlfs(hctrl);
    if (log4c_fini()) {
	    g_message("log4c_fini failed!");
    }
    return 0;
}

