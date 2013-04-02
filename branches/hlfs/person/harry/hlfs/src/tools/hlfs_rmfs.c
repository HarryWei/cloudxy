/*  
 *  Copyright (C) 2013 KangHua <kanghua151@gmail.com>
 *  Copyright (C) 2013 Harry Wei <harryxiyou@gmail.com>
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

static gchar *uri = NULL;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	{"filesystem uri", 'u', 0, G_OPTION_ARG_STRING, &uri, "filesystem uri", "URI"},
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

int hlfs_rmfs(struct back_storage *storage){
        HLOG_DEBUG("enter func %s", __func__);
        int ret = 0;
        uint32_t num_entries;
        bs_file_info_t *infos = storage->bs_file_list_dir(storage, NULL, &num_entries);
        if (infos == NULL) {
                HLOG_ERROR("can not get fs:%s seg entries", storage->uri);
                return -1;
        }
        printf("how much file :%d\n", num_entries);
        int i = 0;
        bs_file_info_t *info = infos;
        for (i = 0;i < num_entries;i++) {
                printf("999 file:%s, size:%llu, time:%llu\n", info->name, \
                                info->size, info->lmtime);  
				storage->bs_file_delete(storage, info->name);
                info++;
		}
		storage->bs_file_delete(storage, NULL);
        printf("leave func %s", __func__);
        free(infos);
        return ret;  
}

/*  mkfs_rmfs -u uri */
int main(int argc, char *argv[])
{
//	if (log4c_init()) {
//		g_message("log4c_init failed!");
//	}
	GError *error = NULL;
	GOptionContext *context;
	context = g_option_context_new("-hlfs rmfs");
	g_option_context_add_main_entries(context, entries, NULL);
	g_option_context_set_help_enabled(context, TRUE);
	g_option_group_set_error_hook(g_option_context_get_main_group(context),
			(GOptionErrorFunc)error_func);
	if (!g_option_context_parse(context, &argc, &argv, &error)) {
		g_message("option parsing failed: %s", error->message);
		exit(EXIT_FAILURE);
	}

	g_print("fs uri is :%s\n", uri);
	int ret = 0;
		
	struct back_storage *storage = init_storage_handler(uri);
	if (NULL == storage) {
		g_message("can not get storage handler for uri:%s", uri);
		ret = -1;
		goto out;
	}
    if (0 != hlfs_rmfs(storage)){
    	g_printf("Can not remove fs!\n");
    	ret = -1;
		goto out1;
    }

out1:
	deinit_storage_handler(storage);
out:
	g_free(uri);
	g_option_context_free(context);
//	if (log4c_fini()) {
//		g_message("log4c_fini failed!");
//	}
	return 0;
}
