/*
  *  Copyright (C) 2012 Harry Wei <harryxiyou@gmail.com>
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
#include "cmd_define.h"
#include "hlfs_log.h"

static gint nbd_server_pid = 0;
static gchar *uri = NULL;
static gchar *cmd = NULL;
static gint   param1 = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	{"nspid",  'p', 0, G_OPTION_ARG_INT,           &nbd_server_pid,   "pid", "PID"},
	{"uri", 'u', 0, G_OPTION_ARG_STRING,        &uri,   "uri", "URI"},
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

/* nbd-ops -p pid -u uri -c command */
int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- nbd ops...");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s\n", error->message);
        exit(EXIT_FAILURE);
    }

    g_message("nspid:       %d\n",nbd_server_pid);
    g_message("uri:      %s\n", uri);
    g_message("cmd:        %s\n", cmd);
    g_message("param1: %d\n" ,param1);
    g_option_context_free(context);
    
    int _pid  = nbd_server_pid;
    char FIFO_R[128];
    char FIFO_W[128];
    snprintf(FIFO_W,128,"%s/%s/%d","/tmp", "nbd-0-",_pid);
    snprintf(FIFO_R,128,"%s/%s/%d","/tmp", "nbd-1-",_pid);
    printf("--FIFO-R :%s\n",FIFO_R);
    printf("--FIFO-W :%s\n",FIFO_W);
    char w_buf[512];
    char r_buf[512];
    int nwrite;
    int msg_w_fd=open(FIFO_W,O_RDWR,0);
    if(msg_w_fd == -1){
       g_message("open   error:   %s/n",   strerror(errno));   
       g_message("open fifo:%s failed\n",FIFO_W);
       exit(-1);
    }
    int msg_r_fd=open(FIFO_R,O_RDWR,0);
    if(msg_r_fd == -1){
       g_message("open   error:   %s/n",   strerror(errno));   
       g_message("open fifo:%s failed\n",FIFO_R);
       exit(-1);
    }

    NBD_OPS_CMD_T nbd_cmd;
    NBD_OPS_RSP_T nbd_rsp;
    if(0 == strncmp(cmd,"start_merge",strlen("start_merge"))){
          g_message("cmd is srart merge : %s",cmd);
          nbd_cmd.nbd_ops_cmd = start_clean;
    }else if(0 == strncmp(cmd,"stop_merge",strlen("stop_merge"))){
          g_message("cmd is stop merge  : %s",cmd);
          nbd_cmd.nbd_ops_cmd = start_clean;
    }else if (0 == strncmp(cmd, "set_copy_waterlevel", strlen("set_copy_waterlevel"))){
	      g_message("cmd is set copy waterlevel: %s", cmd);
	      nbd_cmd.nbd_ops_cmd = set_copy_waterlevel;
    }else if (0 == strncmp(cmd, "query_stat", strlen("query_stat"))){
	      g_message("cmd is query_stat: %s", cmd);
	      nbd_cmd.nbd_ops_cmd = query_stat;
    }else{
          g_assert(0);
    }
    strncpy(nbd_cmd.uri,uri,128);
    nbd_cmd.value = param1;
    g_message("write to fifo value: %d", nbd_cmd.value);
    g_message("write to fifo : %s",nbd_cmd.uri);
    int reval = write(msg_w_fd,&nbd_cmd,sizeof(NBD_OPS_CMD_T));
    if(reval != sizeof(NBD_OPS_CMD_T)) {
       g_message("message send error");
       exit(-1);
    }
    g_message("write over %d",reval);
    sleep(1);
    g_message("read from fifo");
    reval = read(msg_r_fd,&nbd_rsp,sizeof(NBD_OPS_RSP_T));
    if(reval != sizeof(NBD_OPS_RSP_T)){
       g_message("message send error");
       exit(-1);
    }
    g_message("rsponse reval:%d errno:%d",reval,nbd_rsp.err_no);
    if(nbd_rsp.err_no == 0){
       if (0 == strncmp(cmd, "query_stat", strlen("query_stat"))){
          NBD_OPS_STAT nbd_stat;
          reval = read(msg_r_fd,&nbd_stat,sizeof(NBD_OPS_STAT));
          g_message("start clean :%d",nbd_stat.is_start_clean);
          g_message("copy waterlevel :%d",nbd_stat.copy_waterlevel);
       }
    }
    close(msg_w_fd);
    close(msg_r_fd);
    return 0;
}
