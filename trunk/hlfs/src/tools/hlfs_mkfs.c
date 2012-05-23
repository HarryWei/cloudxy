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

static gchar *uri = NULL;
//static gchar *fsname = NULL;
static gint block_size = 0;
static gint seg_size = 0;
static int64_t max_fs_size = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
   //	{"filesystme name", 'f', 0, G_OPTION_ARG_FILENAME, &fsname, "filesystem name", "NAME"},
    	{"filesystem block size", 'b', 0, G_OPTION_ARG_INT, &block_size, "filesystem block size", "BLOCKSIZE"},
    	{"filesystem segment size", 's', 0, G_OPTION_ARG_INT, &seg_size, "filesystem segment size", "SEGSIZE"},
    	{"filesystem max size", 'm', 0, G_OPTION_ARG_INT64, &max_fs_size, "filesystem  max size(M)", "FS MAX SIZE"},
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

    //g_print("location is :%s\n",location);
    //g_print("fsname   is :%s\n",fsname);
    //g_print("block size   is :%d\n",block_size);
    //g_print("segment size   is :%d\n",seg_size);

//  seg_size = SEGMENT_SIZE;
    if(seg_size <= 0 ||  seg_size%(1024*1024)!=0){
       g_message("segsize <=0 or segment size must 1M margin");
       return -1;
    }
    if(block_size <=0 || block_size%(512) != 0){
       g_message("blocksize <=0 or block size must 512 margin");
       return -1;
    }
    if(max_fs_size <=0){
       g_message("max fs size <=0");
       return -1;
    }
    struct back_storage *storage = init_storage_handler(uri);
    if(NULL ==storage){
       g_message("can not get storage handler for uri:%s",uri);
       g_option_context_free(context);
       return -1;
    }
    
    if((0==storage->bs_file_is_exist(storage,NULL)) && (0==storage->bs_file_is_exist(storage,"superblock"))){
        g_message("hlfs with uri:%s has exist",uri);
        g_option_context_free(context);
        return 1;
    }
    if( 0!=storage->bs_file_mkdir(storage,NULL)){
        g_message("can not mkdir for our fs %s",uri);
        g_option_context_free(context);
        return -1;
    }

    GKeyFile *sb_keyfile= g_key_file_new();
    g_key_file_set_string(sb_keyfile,"METADATA","uri",uri);
    g_key_file_set_integer(sb_keyfile,"METADATA","block_size",block_size);
    g_key_file_set_integer(sb_keyfile,"METADATA","segment_size",seg_size);
    g_key_file_set_uint64(sb_keyfile,"METADATA","max_fs_size",max_fs_size);
    gchar *data = g_key_file_to_data(sb_keyfile,NULL,NULL);
    g_message("key file data :%s",data);
    char *head,*hostname,*dir,*fs_name;
    int port;
    parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
    char *sb_file_path = g_build_filename(dir,fs_name,"superblock",NULL);
    g_message("sb file path %s",sb_file_path);
    bs_file_t file = storage->bs_file_create(storage,"superblock");
    g_message("sb file path 1%s",sb_file_path);
    //bs_file_t file = storage->bs_file_open(storage,"superblock",BS_WRITEABLE);
    if (NULL == file) {
       g_message("open file :superblock failed");
       g_free(sb_file_path);
       g_option_context_free(context);
       return -1;
    }

    g_message("sb file path 2%s",sb_file_path);
    int size = storage->bs_file_append(storage, file,(char*)data,strlen(data)+1);
    if(size != strlen(data)+1){
       g_message("can not write superblock file");
       g_free(sb_file_path);
       g_option_context_free(context);
       return -1;
    }
    g_message("append size:%d",size);
    storage->bs_file_flush(storage,file);
    storage->bs_file_close(storage,file);
    deinit_storage_handler(storage);
    if (log4c_fini()) {
	    g_message("log4c_fini failed!");
    }
    return 0;
}
