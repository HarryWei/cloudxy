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
#include "snapshot.h"
static gchar *son_uri = NULL;
static gchar *father_uri_with_snapshot = NULL;
static gchar *father_uri =NULL;
static gchar *father_snapshot =NULL;
//static gchar *fsname = NULL;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	{"filesystem father loc with snapshot",'f', 0, G_OPTION_ARG_STRING,&father_uri_with_snapshot, "father storage uri with snapshot", "SFSLOC"},
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
    g_message("father_uri_with_snapshot:%s;son_uri:%s",father_uri_with_snapshot,son_uri);
    gchar **v=NULL;
    v = g_strsplit(father_uri_with_snapshot,"%",2);
    if(g_strv_length(v)!=2){
       g_strfreev(v);
	   g_message("not give snapshot for base:%s",father_uri_with_snapshot);
       return -1; 
    }
	
    father_uri = g_strdup(v[0]);
    father_snapshot = g_strdup(v[1]);
    g_strfreev(v);

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
    gchar   * _father_uri =  g_key_file_get_string(sb_keyfile,"METADATA","father_uri",NULL);
    
    printf("father uri: %s\n",_father_uri);
    if(_father_uri != NULL){
         g_message("uri:%s has clone :%s",son_uri,_father_uri);
         return -1;
    }
    g_free(content);
	
    /*  read father's snapshot 's inode */
    uint64_t inode_addr;
#if 0
    if(0 != get_cur_latest_segment_info(father_storage,&segno,&offset)){
        g_message("can not get father's latest segment info");
        return -1;
    }else{
        if(segno == 0 && offset == 0){
           g_message("father hlfs is empty,no meaning");
           return -1;
        }else{
           g_message("father lastest segno:%d,offset:%d",segno,offset);
           struct inode_map_entry imap_entry;
           if( 0 != load_latest_inode_map_entry(father_storage,segno,offset,&imap_entry)){
               g_message("can not get latest inode map entry from father hlfs");
               return -1;
           } 
           inode_addr = imap_entry.inode_addr;
        }
    }
#else
       struct snapshot *ss = NULL;
       if (0 > load_snapshot_by_name(father_storage, SNAPSHOT_FILE, &ss, father_snapshot)) {
		g_message("load uri:%s ss by name:%s error",father_uri,father_snapshot);
		g_free(ss);
		return -1;
	}
	inode_addr = ss->inode_addr;
	g_free(ss);
	
#endif 
    uint32_t    father_seg_size = 0;
    uint32_t    father_block_size = 0;
    uint64_t    father_max_fs_size = 0;
    if (0 != read_fs_meta(father_storage,&father_seg_size,&father_block_size,&father_max_fs_size)){
	  g_message("can not read father uri meta");	
	  return -1;	
    }
    segno = get_segno(inode_addr);
    uint32_t son_block_size =  g_key_file_get_integer(sb_keyfile,"METADATA","block_size",NULL);
    uint32_t son_seg_size =  g_key_file_get_integer(sb_keyfile,"METADATA","segment_size",NULL); 	
    if (son_block_size != father_block_size || father_seg_size!=son_seg_size){
	  g_message("sorry , now father segsize and block sizee must same as son!!!");	
	  return -1;	
    }		
    g_key_file_set_uint64(sb_keyfile,"METADATA","from_segno",segno+1);
    g_key_file_set_string(sb_keyfile,"METADATA","father_uri",father_uri);
    g_key_file_set_string(sb_keyfile,"METADATA","father_ss",father_snapshot);	
    g_key_file_set_uint64(sb_keyfile,"METADATA","snapshot_inode",inode_addr);
    g_key_file_set_uint64(sb_keyfile,"METADATA","max_fs_size",father_max_fs_size);	
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
