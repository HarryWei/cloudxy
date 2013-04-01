/*
  *  Copyright (C) 2012      KangHua <kanghua151@gmail.com> 
  *  Copyright (C) 2012      Harry Wei <harryxiyou@gmail.com> 
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <unistd.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <sys/mman.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"

static gchar *fsname = NULL;
static gchar *cmd = NULL;
static gint   param1 = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	{"fsname", 'f', 0, G_OPTION_ARG_STRING,        &fsname,   "fsname", "FSNAME"},
	{"cmd",    'c', 0, G_OPTION_ARG_STRING,  &cmd,   "command", "COMMAND"},
	{"param1", 'r', 0, G_OPTION_ARG_INT, &param1, "param1", "PARAM1"},
    {"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "be verbose", NULL },
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

/* nbd-ops -p pid -f fsname -c command */
int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- tapdisk ops...");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s\n", error->message);
        exit(EXIT_FAILURE);
    }

    g_message("fsname:      %s\n", fsname);
    g_message("cmd:        %s\n", cmd);
    g_message("param1: %d\n" ,param1);
    g_option_context_free(context);
    char ctrl_region_file[128];
    sprintf(ctrl_region_file,"%s%s%s","/tmp/",fsname,"-ctrl");
    int fd = open(ctrl_region_file,O_RDWR);
    if(fd == -1){
        return -1;
    }
//    int offset = sysconf(_SC_PAGE_SIZE);
    char *addr = mmap(NULL,sizeof(CTRL_REGION_T), PROT_WRITE, MAP_SHARED, fd, 0);
    if (addr == MAP_FAILED) {
	    g_message("%s -- mmap failed\n", __func__);
	    exit(EXIT_FAILURE);
    }
    g_assert(addr !=NULL);
    CTRL_REGION_T *ctrl_region;      
    ctrl_region = (CTRL_REGION_T*)addr;
    gint *_is_start_clean  = &ctrl_region->is_start_clean;
    gint *_copy_waterlevel = &ctrl_region->copy_waterlevel;
    if(0 == strncmp(cmd,"start_merge",strlen("start_merge"))){
        g_message("cmd is srart merge : %s",cmd);
        g_atomic_int_set(_is_start_clean,1);
    }else if(0 == strncmp(cmd,"stop_merge",strlen("stop_merge"))){
        g_message("cmd is stop merge  : %s",cmd);
        g_atomic_int_set(_is_start_clean,0);
    }else if (0 == strncmp(cmd, "set_copy_waterlevel", strlen("set_copy_waterlevel"))){
        g_message("cmd is set copy waterlevel: %s", cmd);
        g_atomic_int_set(_copy_waterlevel,param1);
    }else if (0 == strncmp(cmd, "query_stat", strlen("query_stat"))){
        g_message("cmd is query_stat: %s", cmd);
        g_message("is_start_clean:%d", ctrl_region->is_start_clean);
        g_message("copy_waterlevel:%d",ctrl_region->copy_waterlevel);
    }else{
        g_assert(0);
    }
    return 0;
}
