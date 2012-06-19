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
#include "comm_define.h"
#include "hlfs_log.h"
#include "api/hlfs.h"

static gchar *uri = NULL;
//static gchar *fsname = NULL;
static int start_segno;
static int end_segno;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem uri",'u', 0, G_OPTION_ARG_STRING,   &uri,          "filesystem storage uri", "FSLOC"},
	    {"start_segno",  's', 0, G_OPTION_ARG_INT,      &start_segno, "start seg no", "START"},
	    {"end_segno",    'e', 0, G_OPTION_ARG_INT,      &end_segno,   "end seg no",   "END"},
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

/*segcalc.hlfs -u uri -v verbose*/
int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- seg usage calc");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            (GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }
    g_option_context_free(context);
    g_message("uri is :%s,start segno:%d,end segno:%d",uri,start_segno,end_segno);
    //g_message("fsname   is :%s",fsname);

    struct back_storage *storage = init_storage_handler(uri);
    if(NULL ==storage){
        g_message("can not get storage handler for uri:%s",uri);
        return -1;
    }
    g_message("storage init over....");
    uint32_t segment_size;
    uint32_t block_size;
    uint64_t max_fs_size;
    int ret = read_fs_meta(storage, &segment_size, &block_size,&max_fs_size);
    g_message("segment size:%d,block size:%d,max fs size%d",segment_size,block_size,max_fs_size);
#if 0
    SEGMENT_SIZE_MASK  = segment_size - 1;
    SEGMENT_SIZE_SHIFT = 0;
    while (0 != (segment_size = (segment_size >> 1)))
    {                                                                                                         
        SEGMENT_SIZE_SHIFT++;
    }
#endif
#if 0
    struct inode * latest_inode = load_latest_inode(storage); 
    g_message("latest inode's timestamp :%llu",latest_inode->ctime);
    int num_entries;
    bs_file_info_t * infos = storage->bs_file_list_dir(storage,".",&num_entries);
    if(infos == NULL){
        g_message("can not get fs:%s seg entries",storage->uri);
        return -1;
    }
    g_message("there are %d files",num_entries);
    bs_file_info_t * info = infos;
    int i;
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    ret = load_all_seg_usage(storage,SEGMENTS_USAGE_FILE,seg_usage_hashtable);
    g_assert(ret == 0 );

    uint32_t active_segno;
    uint32_t active_offset;
    ret = get_cur_latest_segment_info(storage,&active_segno,&active_offset);
    if(ret !=0){
        return -1;
    }
    g_message("active segno :%u",active_segno);
    for(i=0;i<num_entries;i++){
        g_message("file:%s,size:%llu,time:%llu",info->name,info->size,info->lmtime);  
        if(g_str_has_suffix(info->name,"seg")){
            g_message("valid seg file:%s",info->name);
            uint32_t segno = get_segfile_no(info->name);
            g_message("segno %u",segno);
            if(segno == active_segno){
                info++;
                continue; 
            }
            gpointer tmp = g_hash_table_lookup(seg_usage_hashtable,GINT_TO_POINTER(segno));
            struct segment_usage *seg_usage;
            if(tmp == NULL){
                g_message("can not find seg:%u in segment usage file",segno);
                seg_usage = (struct segment_usage*)g_malloc0(sizeof(struct segment_usage));
                memset(seg_usage,0,sizeof(struct segment_usage));
#if 0
                gchar * segusage_file = g_strconcat(info->name,".usage",NULL);
                dump_segment_usage(storage,segusage_file,&seg_usage);
#else
            }else{
                seg_usage = (struct segment_usage*)tmp;
            }
#endif 
            seg_usage_calc(storage,info->name,latest_inode,seg_usage,block_size);
            dump_seg_usage(storage,SEGMENTS_USAGE_FILE,seg_usage);

            g_free(seg_usage->bitmap);
            g_free(seg_usage);
        } 
        info++;
    }
    g_hash_table_destroy(seg_usage_hashtable);
    free(infos);
#endif

    GList* ss_list = NULL;
    if(0 == storage->bs_file_is_exist(storage,SNAPSHOT_FILE)){
        GHashTable   *ss_hashtable = g_hash_table_new_full(g_str_hash,g_str_equal,NULL,NULL);
        ret = load_all_snapshot(storage,SNAPSHOT_FILE,ss_hashtable);
        g_message("snapshot loaded"); 
        //g_assert(ret == 0);
        if(ret !=0){
            g_message("load snapshot failed"); 
            g_assert(0);
        }		
        ret = sort_all_snapshot(ss_hashtable,&ss_list);
        g_message("snapshot sorted"); 
        g_assert(ss_list !=NULL);
    }else{
        g_message("snapshot not exist"); 
    }
    g_assert(ret == 0);
    struct inode * inode=NULL;
    SEG_USAGE_T seg_usage;
    memset(&seg_usage,0,sizeof(SEG_USAGE_T));
    int i;
    for(i=start_segno;i<=end_segno;i++){
        memset(&seg_usage,0,sizeof(SEG_USAGE_T));
        char segfile[128];
        build_segfile_name(i, segfile);
        if(0!=storage->bs_file_is_exist(storage,segfile)){
            g_message("segfile:%s has remove or has not create",segfile); 
            continue;
        }
        struct inode * inode=NULL;
        char *up_sname;
        if(ss_list != NULL){
            ret = get_refer_inode_between_snapshots(storage,i,ss_list,&inode,&up_sname);
        }else{
            g_message("not snapshot,so do above snapshot !!!"); 
            ret = ABOVE_SNAPSHOT;
        }	
        g_message("segno :%d ret:%d",i,ret);
        if(ret == ON_SNAPSHOT){
            g_message("seg is on snapshot,do nothing");
            continue;
        }else if (ret == IN_SNAPSHOT){
            g_message("seg is in snapshots");
            strncpy(seg_usage.up_sname,up_sname,strlen(up_sname));
        }else if(ret == ABOVE_SNAPSHOT){
            g_message("seg is above snapshot");
            strncpy(seg_usage.up_sname,EMPTY_UP_SNAPSHOT,strlen(EMPTY_UP_SNAPSHOT));
            g_message("up sname is:%s",seg_usage.up_sname);
            inode  = load_latest_inode(storage); 
        }else{
            g_message("get refer inode failed");
            continue;
        }
        ret = seg_usage_calc(storage,HBLOCK_SIZE,i,inode,&seg_usage);
        if(ret != 0){
            g_message("seg_usage_calc failed");
            g_assert(0);
        }
        char textbuf[8192];
        memset(textbuf,0,8192);
        ret = seg_usage2text(&seg_usage,textbuf);
        g_assert(ret > 0);
        g_message("textbuf is :%s",textbuf);
        ret = dump_seg_usage(storage,SEGMENTS_USAGE_FILE,&seg_usage);
        g_assert(ret == 0);
    }
    return ret;
}
