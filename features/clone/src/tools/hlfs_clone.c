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

static gchar *son_uri = NULL;
static gchar *father_uri = NULL;
//static gchar *fsname = NULL;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem father loc",'f', 0, G_OPTION_ARG_STRING,&father_uri, "father storage uri", "SFSLOC"},
	    {"filesystem self   loc",'s', 0, G_OPTION_ARG_STRING,&son_uri, "son storage uri", "SFSLOC"},
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


int main(int argc, char *argv[])
{
	if (log4c_init()) {
		g_message("log4c_init failed!");
	}
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- clone hlfs");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }

    g_option_context_free(context);
    g_message("father_uri:%s;son_uri:%s",father_uri,son_uri);

    if(0 == strcmp(father_uri,son_uri)){
        g_message("father uri can not equal son uri");
        return -1;
    }
    struct back_storage *father_storage = init_storage_handler(father_uri);
    if(NULL ==father_storage){
       g_message("can not get storage handler for father_uri:%s",father_uri);
       return -1;
    }

    struct back_storage *son_storage = init_storage_handler(son_uri);
    if(NULL ==son_storage){
       g_message("can not get storage handler for son_uri:%s",son_uri);
       return -1;
    }
    /*  check son is exist and empty must  */
    if((0!=son_storage->bs_file_is_exist(son_storage,NULL)) || (0!=son_storage->bs_file_is_exist(son_storage,"superblock"))){
        g_message("hlfs with uri:%s has not exist,please mkfs it first!",son_uri);
        return -1;
    }
    uint32_t segno=0;
    uint32_t offset=0;
    if(0!=get_cur_latest_segment_info(son_storage,&segno,&offset)){
        g_message("can not get latest seg info for son ");
    }else{
        if(segno != 0 || offset != 0){
           g_message("son hlfs must empty");
           return -1; 
        }
    }
    /* check son is not clone already */
    char *content =NULL;
    uint32_t size = 0;
    if(0 != file_get_contents(son_storage,"superblock",&content,&size)){
         g_message("can not read superblock");
    }
    GKeyFile * sb_keyfile = g_key_file_new();
    if(FALSE == g_key_file_load_from_data(sb_keyfile,content,size,G_KEY_FILE_NONE,NULL)){
         g_message("superblock file format is not key value pairs");
         return -1;
    }
    gchar * _father_uri =  g_key_file_get_string(sb_keyfile,"METADATA","father_uri",NULL);
    printf("father uri: %s\n",_father_uri);
    if(_father_uri != NULL){
         g_message("uri:%s has clone :%s",son_uri,_father_uri);
         return -1;
    }
    g_free(content); 
    /* read father's latest inode */
    uint64_t inode_addr;
    if(0 != get_cur_latest_segment_info(father_storage,&segno,&offset)){
        g_message("can not get father's latest segment info");
        return -1;
    }else{
        if(segno == 0 && offset == 0){
           g_message("father hlfs is empty,no meaning");
           return -1;
        }else{
           struct inode_map_entry imap_entry;
           if( 0 != load_latest_inode_map_entry(father_storage,segno,offset,&imap_entry)){
               g_message("can not get latest inode map entry from father hlfs");
               return -1;
           } 
           inode_addr = imap_entry.inode_addr;
        }
    }
    g_key_file_set_uint64(sb_keyfile,"METADATA","from_segno",segno+1);
    g_key_file_set_string(sb_keyfile,"METADATA","father_uri",father_uri);
    g_key_file_set_uint64(sb_keyfile,"METADATA","base_father_inode",inode_addr);
    gchar *data = g_key_file_to_data(sb_keyfile,NULL,NULL);
    g_message("key file data :%s",data);
    if(0 != son_storage->bs_file_delete(son_storage,"superblock")){
       g_message("can not delete old superblock file");
       return -1; 
    }
    if(0 != file_append_contents(son_storage,"superblock",(char*)data,strlen(data)+1)){
       g_message("can not write superblock file");
       return -1;
    }
    deinit_storage_handler(son_storage);
    deinit_storage_handler(father_storage);
    if (log4c_fini()) {
	    g_message("log4c_fini failed!");
    }
    return 0;
}
