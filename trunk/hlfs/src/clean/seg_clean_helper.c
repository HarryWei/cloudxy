#include <stdio.h>
#include <time.h>
#include <sys/time.h>
/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <stdlib.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage_helper.h"
#include "storage.h"
#include "snapshot.h"
#include "seg_clean.h"

int seg_usage2text(SEG_USAGE_T * seg_usage,char *textbuf){
       HLOG_DEBUG("enter func %s",__func__);
       memset(textbuf,0,sizeof(SEG_USAGE_T)*10);
       int n = sprintf (textbuf,"%llu %s %llu %llu %u %u %u ",
		       seg_usage->segno,
		       seg_usage->up_sname,
		       seg_usage->inode_addr,
		       seg_usage->timestamp,
		       seg_usage->log_num,
		       seg_usage->block_num,
		       seg_usage->alive_block_num);
       //HLOG_DEBUG("textbuf init:%s, n:%d",textbuf,n);
	   int m=0;
       int i,j;
	   for(i=0;i<((seg_usage->log_num-1)/sizeof(gint))+1;i++){
           //for(j=0;j<8;j++){
	           //gboolean value = (segment_usage->bitmap[i] & (1<<(8 - j))) != 0;
	           //m += sprintf(textbuf + n + m,"%x ",value);
               //if(value==FALSE)
               //   printf("value:%d , offset m:%d\n",value,m);
           //}
           m += sprintf(textbuf+n+m,"%hhx#",seg_usage->bitmap[i]);
	   }
       sprintf(textbuf+n+m,"\n");
       //HLOG_DEBUG("leave func %s",__func__);
	   return n+m+1;
}


#if 0
int dump_seg_usage_text(struct back_storage * storage,const char*segment_usage_file, const char *seg_usage_text){
    HLOG_DEBUG("enter func %s",__func__);
    HLOG_DEBUG("enter func %s,seg usage file:%s",__func__,segment_usage_file);
    int ret = 0;
    uint32_t len;
    /*  TODO -- do not create runtime */
    bs_file_t file = NULL;
    if(-1 == storage->bs_file_is_exist(storage,segment_usage_file)){
        g_message("segment usage file not exist, create it");
        file = storage->bs_file_create(storage,segment_usage_file); 
        if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segment_usage_file);
            goto out; 
        }
        storage->bs_file_close(storage,file);
    }
    
    file = storage->bs_file_open(storage,segment_usage_file,BS_WRITEABLE);
    if(file == NULL){
        HLOG_ERROR("can not open segment usage file %s",segment_usage_file);
        goto out;
    }
    len = strlen(seg_usage_text);
    HLOG_DEBUG("segtextbuf is %s",seg_usage_text);
    if(0 > (ret = storage->bs_file_append(storage,file,seg_usage_text,len))){
        HLOG_ERROR("write seg usage faied:%d",ret);
        ret = -1;
        goto out;
    }
out:
    if(file!=NULL)
      storage->bs_file_close(storage,file);
       HLOG_DEBUG("leave func %s",__func__);
	return ret;
  
}
#endif 

#if 0
int segment_delmark2text(uint32_t segno,char *textbuf){
       HLOG_DEBUG("enter func %s",__func__);
       uint32_t n = sprintf(textbuf,"%u\n",segno);
       HLOG_DEBUG("leave func %s",__func__);
	   return n;
}
int dump_segment_delmark(struct back_storage* storage,const char* segment_delmark_file,uint32_t segno){
    /*  TODO -- do not create runtime */
       HLOG_DEBUG("enter func %s",__func__);
    HLOG_DEBUG( "enter func %s,seg del mark file:%s",__func__,segment_delmark_file);
    int ret = 0;
    char segtextbuf[128];
    uint32_t len;
    bs_file_t file = NULL;
    if(-1 == storage->bs_file_is_exist(storage,segment_delmark_file)){
        g_message("segment usage file not exist, create it");
        file = storage->bs_file_create(storage,segment_delmark_file); 
        if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segment_delmark_file);
            goto out; 
        }
        storage->bs_file_close(storage,file);
    }
    file = storage->bs_file_open(storage,segment_delmark_file,BS_WRITEABLE);
    if(file == NULL){
        HLOG_ERROR("can not open segment usage file %s",segment_delmark_file);
        goto out;
    }

    memset(segtextbuf,0,128);
    len = segment_delmark2text(segno,segtextbuf);
    HLOG_DEBUG("segtextbuf is %s",segtextbuf);
    if(0 > (ret = storage->bs_file_append(storage,file,segtextbuf,len))){
        HLOG_ERROR("write seg usage faied:%d",ret);
        ret = -1;
    }
out:
    if(file!=NULL)
        storage->bs_file_close(storage,file);
       HLOG_DEBUG("leave func %s",__func__);
    return ret;
}

#endif 



int seg_usage4text(SEG_USAGE_T * seg_usage, const char *textbuf){
     if(seg_usage == NULL || textbuf == NULL){
	    return -1; 
     }
     //HLOG_DEBUG("enter func %s",__func__);
     //HLOG_DEBUG("textbuf len:%d",strlen(textbuf));
     //HLOG_DEBUG("textbuf :%s",textbuf);
     gchar **v = g_strsplit (textbuf," ",1024);
     gchar *_segno_str = v[0]; 
     gchar *_up_sname = v[1];
     gchar *_inode_addr_str = v[2]; 
     gchar *_timestamp_str = v[3];
     gchar *_log_num_str = v[4];
     gchar *_block_num_str = v[5];
     gchar *_alive_block_str = v[6]; 
     gchar *bitmap_str = g_strdup(v[7]);
     HLOG_DEBUG("segno str:%s,up sname:%s,inode addr:%s,log num:%s,block num:%s,alive block str:%s",
	 		     _segno_str,
	 		     _up_sname,
	 		     _inode_addr_str,
	 		     _log_num_str,
	 		     _block_num_str,
	 		     _alive_block_str);

     char *endptr = NULL;
     seg_usage->segno = strtoull(_segno_str,&endptr,0);
     strncpy(seg_usage->up_sname,_up_sname,strlen(_up_sname));
     seg_usage->inode_addr = strtoull(_inode_addr_str,&endptr,0);
     seg_usage->timestamp = strtoull(_timestamp_str,&endptr,0);
     //seg_usage->log_num = strtoull(_log_num_str,&endptr,0);
     seg_usage->log_num = strtol(_log_num_str,&endptr,0);
     seg_usage->alive_block_num = strtol(_alive_block_str,&endptr,0);
     seg_usage->block_num = strtol(_block_num_str,&endptr,0);
    
	 
     g_strfreev(v);
     HLOG_DEBUG("segno:%llu,alive_blocks:%u,timestamp:%llu,log_num:%u,block_num:%u",
		   		 seg_usage->segno,
                 seg_usage->alive_block_num,
                 seg_usage->timestamp,
                 seg_usage->log_num,
                 seg_usage->block_num);
     seg_usage->bitmap = (char*)g_malloc0((seg_usage->log_num - 1)/sizeof(gint) +1);
     HLOG_DEBUG("bitmap_str :%s",bitmap_str);
     v = g_strsplit(bitmap_str,"#",1024);
     int i;
     for(i=0;i<g_strv_length(v) - 1;i++){
         seg_usage->bitmap[i] = strtoul(v[i],&endptr,16);
     }
     g_strfreev(v);
     //HLOG_DEBUG("leave func %s",__func__);
     return 0;
}




int get_refer_inode_between_snapshots(struct back_storage *storage,uint32_t segno,GList *snapshot_sorted_list, struct inode ** inode,char **up_sname){
    if(storage == NULL || snapshot_sorted_list == NULL){
        return -1; 
    }
    int num_entries = g_list_length(snapshot_sorted_list);
    HLOG_DEBUG("snapshot count :%d",num_entries);	
    HLOG_DEBUG("segno :%d",segno);	
    struct snapshot * cur_snapshot;
    struct snapshot * pre_snapshot;
    cur_snapshot = (struct snapshot *)g_list_nth_data(snapshot_sorted_list,0);
    pre_snapshot = cur_snapshot;
    if( segno < get_segno(cur_snapshot->inode_addr)){
        HLOG_DEBUG("seg in first range");	
        *inode =  load_inode(storage,cur_snapshot->inode_addr);
	  if(*inode == NULL){
		   HLOG_DEBUG("load inode failed");
		   return -1;
	  }	 		
        *up_sname = strdup(cur_snapshot->sname);
        return IN_SNAPSHOT;
    }
    int i;
    for(i=1;i<num_entries;i++){
        HLOG_DEBUG("idx:%d,pre snapshot inode segno:%u,name:%s;cur snapshot inode segno:%u,name:%s",i,get_segno(pre_snapshot->inode_addr),pre_snapshot->sname,get_segno(cur_snapshot->inode_addr),cur_snapshot->sname);	
        if ( segno > get_segno (pre_snapshot->inode_addr) && segno < get_segno(cur_snapshot->inode_addr) ){
            *inode =  load_inode(storage,cur_snapshot->inode_addr);
	     if(*inode == NULL){
		   HLOG_DEBUG("load inode failed");
		   return -1;
	     }	 	
            *up_sname = strdup(cur_snapshot->sname);
            return IN_SNAPSHOT;
        }else if (segno == get_segno(cur_snapshot->inode_addr)){
            return ON_SNAPSHOT;
        }
        pre_snapshot=cur_snapshot;
        cur_snapshot = (struct snapshot *)g_list_nth_data(snapshot_sorted_list,i);
    }
    return ABOVE_SNAPSHOT;
}

int load_all_seg_usage(struct back_storage *storage,
						  const char   * seg_usage_file,
						  GHashTable   * seg_usage_hashtable){
    HLOG_DEBUG("enter func %s",__func__);
	char *content = NULL;
	uint32_t size = 0;
	if(0!= file_get_contents(storage,seg_usage_file,&content,&size)){
	    HLOG_ERROR("read segfile:%s failed",seg_usage_file);
		return -1;
    }		
    HLOG_DEBUG("read seg usage file size:%u,content len:%d",size,strlen(content));
#if 1
    gchar ** segs = g_strsplit(content,"\n",0);
    HLOG_DEBUG("there are %d segno in segment usage file",g_strv_length(segs));
    int i;
    for(i=0;i<g_strv_length(segs)-1;i++){
        if(strlen(segs[i])<=1){
           continue; 
        }
        HLOG_DEBUG("seg usage content:%s",segs[i]);
        SEG_USAGE_T * seg_usage= (SEG_USAGE_T *)g_malloc0(sizeof(SEG_USAGE_T));
        seg_usage4text(seg_usage,segs[i]);
        //g_hash_table_insert(seg_usage_hashtable,seg_usage->segno,seg_usage);
        HLOG_DEBUG("segno:%llu",seg_usage->segno);
#if 0
        if(seg_usage->alive_block_num !=0){
            g_hash_table_insert(seg_usage_hashtable,GINT_TO_POINTER((uint32_t) seg_usage->segno),seg_usage);
        }else{
            HLOG_DEBUG("segno:%llu can been delete ",seg_usage->segno);

            g_free(seg_usage);
        }
#endif
        SEG_USAGE_T *_seg_usage = g_hash_table_lookup(seg_usage_hashtable,GINT_TO_POINTER((uint32_t)seg_usage->segno));
        if(_seg_usage!=NULL){
           HLOG_DEBUG(" seg usage has exit,replace it");
           g_hash_table_replace(seg_usage_hashtable,GINT_TO_POINTER((uint32_t) seg_usage->segno),seg_usage);
           g_free(_seg_usage->bitmap);
           g_free(_seg_usage);
        }else{
           g_hash_table_replace(seg_usage_hashtable,GINT_TO_POINTER((uint32_t) seg_usage->segno),seg_usage);
        }
    }
    g_strfreev(segs);
#else 
    char *line = textbuf;
    int offset = 0;
    do{
        HLOG_DEBUG("........line:%s,len:%d",line,strlen(line));
        int len = strlen(line);
        line += len+1;
        offset += len+1 ;
    }while(offset < count);
#endif
    HLOG_DEBUG("leave func %s",__func__);
    return 0;
}


static int compare_seg_usage(gconstpointer litem,
                        gconstpointer ritem){
        int ret = 0;
		SEG_USAGE_T *ls = (SEG_USAGE_T *)litem;
		SEG_USAGE_T *rs = (SEG_USAGE_T *)ritem;
        if(ls->segno < rs->segno){
           ret = -1;
        }else if(ls->segno > rs->segno){
           ret =  1;
        }else{
           ret =  0;
        }
     return ret;
}

int sort_all_seg_usage(GHashTable *su_hashtable,GList **su_list){
    (*su_list) = g_hash_table_get_values(su_hashtable);
    (*su_list) = g_list_sort((*su_list),compare_seg_usage);
    return 0;
}



