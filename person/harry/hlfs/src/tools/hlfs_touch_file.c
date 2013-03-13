/*
  *  Copyright (C) 2012 Harry Wei <harryxiyou@gmail.com>
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
#include "dentry.h"
#include "hlfs_log.h"
#include "api/hlfs.h"
#include "hlfs_log.h"

static gchar *uri = NULL;
static gchar *file_name = NULL;
static int is_dir;

static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	 {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
      	 {"file name", 'f', 0, G_OPTION_ARG_STRING, &file_name, "file name", "FNAME"},
      	 {"is dir", 'd', 0, G_OPTION_ARG_INT, &is_dir, "is dir", "ISDIR"},
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

/*  touch.hlfs -u local<hdfs>://xxx/xxx -f file_name */
int main(int argc, char *argv[])
{
	if (log4c_init()) {
		g_message("log4c_init failed!");
	}
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- touch hlfs file");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }

    g_print("location is :%s\n", uri);
    g_print("file name is :%s\n", file_name);
    g_print("is_dir is :%d\n", is_dir);
    g_option_context_free(context);
    HLFS_CTRL *hctrl = init_hlfs(uri);
    g_assert(hctrl != NULL);
	int ret = 0;
	if (is_dir != HLFS_DIR && is_dir != HLFS_FILE) {
		g_message("is_dir must be 1(hlfs file) or 0(hlfs dir).");
		ret = -1;
		goto out;
	}
#if 0
    if(0 != hlfs_open(hctrl,1)){
	 g_message("can not hctrl:%s",uri);
	 return -1;
    }		
#endif
    if(0 != hlfs_create(hctrl, file_name, is_dir) ){
	 g_message("cant not create file:%s", file_name);
	 return -1;
    }
out:
    hlfs_fclose(hctrl, file_name);
    deinit_hlfs(hctrl);
    if (log4c_fini()) {
	    g_message("log4c_fini failed!");
    }
    return 0;
}

