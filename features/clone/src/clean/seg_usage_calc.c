/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <glib.h>
#include "logger.h"
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage.h"
#include "seg_clean.h"


int seg_usage_calc(struct back_storage* storage,uint32_t block_size,uint32_t segno,struct inode *refer_inode,SEG_USAGE_T *seg_usage)
{
    //HLOG_DEBUG("enter func %s",__func__);
    if(storage == NULL || refer_inode == NULL || seg_usage == NULL){
		HLOG_ERROR("input params failed");
		return -1;
    }
#if 0
	if(seg_usage->bitmap!=0 && segno != seg_usage->segno){
		HLOG_ERROR("segno not match");
		return -1;
	}	
#endif	
    int ret = 0;
    int log_idx=0;
    int idx;
    uint32_t offset = 0; 
    struct log_header *lh;
    //gchar **v = g_strsplit(segfile,".",2);
    // uint64_t db_mine_storage_addr_segno = atol(v[0]);
    //g_strfreev(v);

    GArray *tmp_bit_array;
    seg_usage->segno = segno; 
    seg_usage->timestamp = get_current_time();
    HLOG_DEBUG("seg usage's segno:%llu,timestamp:%llu",seg_usage->segno,seg_usage->timestamp);

    char segfile[SEGMENT_FILE_NAME_MAX];
	build_segfile_name(segno,segfile);
	char *content = NULL;
	uint32_t size;
	if(0!= (ret = file_get_contents(storage,segfile,&content,&size))){
	    HLOG_ERROR("read segfile:%s failed",segfile);
		return -1;
	}		

    tmp_bit_array = g_array_new(FALSE,FALSE,sizeof(gint));
	

    while(offset < size){
#if 0
        ret=storage->bs_file_pread(storage,file, (char*)&lh, LOG_HEADER_LENGTH, offset) ;//TODO read 64M once
        g_message("read content len:%d\n",ret);
        if(ret<0){
            ret = -1;
            goto out;
        }else if (ret == 0){
            g_message("read over?\n");
            ret = 0;
            break;
        }else if (ret == LOG_HEADER_LENGTH){
            g_message("read log header over\n");
            ;
        }else{
            g_message("read log header failed\n");
            ret = -1;
            goto out;
        }
#endif 
        lh = (struct log_header*)(content + offset);
        if(seg_usage->log_num !=0){
            //HLOG_DEBUG("this segfile:%s has calc",segfile);
            idx = log_idx/sizeof(gint);
            if(!(seg_usage->bitmap[idx] & (1<<(log_idx%sizeof(gint))))){
                log_idx++;
                int x=0;
                g_array_append_val(tmp_bit_array,x);
                offset += lh->log_size;
                continue;
            }
        }
        uint32_t orgine_alive_block_num = seg_usage->alive_block_num;
        //HLOG_DEBUG("start db no:%llu,db num:%d",lh->start_db_no,lh->db_num);
        int i;
#if 1 /* check refer inode whether still refer to given db in seg */
        for(i=0;i<lh->db_num;i++){
            //HLOG_DEBUG("for db:%llu",lh->start_db_no+i);
            uint64_t db_mine_storage_addr = 0;
            uint32_t db_mine_storage_addr_offset = offset + LOG_HEADER_LENGTH+i*block_size;
            set_offset(&db_mine_storage_addr,db_mine_storage_addr_offset);
            set_segno (&db_mine_storage_addr,segno);
            uint64_t db_cur_storage_addr = get_db_storage_addr_in_inode(storage,refer_inode,
			    						                                lh->start_db_no+i,block_size);
            HLOG_DEBUG("db:%llu's mine storage addr:%llu,cur storage addr:%llu",
			    	lh->start_db_no+i,db_mine_storage_addr,db_cur_storage_addr);
            if(db_mine_storage_addr != db_cur_storage_addr){
                HLOG_DEBUG("this is overwrite data block");
            }else{
                seg_usage->alive_block_num++;
                HLOG_DEBUG("this is used data block :%llu",seg_usage->alive_block_num);
            }
			seg_usage->block_num++;
        }
#endif
        //uint32_t alive_blocks = log_usage_calc(storage,latest_inode,&lh,db_mine_storage_addr_segno,offset,block_size);
        if(orgine_alive_block_num == seg_usage->alive_block_num){
            HLOG_DEBUG("log:%d has not any datablock",log_idx);
            //uint32_t bitmap_idx = log_idx / ALIVE_LOG_BITMAP ;
            //seg_usage->alive_log_bitmap[bitmap_idx] &= ~(1 << (log_idx % sizeof(uint64_t)));
            int x=0;
            g_array_append_val(tmp_bit_array,x);
        }else{
            HLOG_DEBUG("log:%d has any datablock",log_idx);
            int x=1;
            g_array_append_val(tmp_bit_array,x);
        }
        offset += lh->log_size;
        log_idx++;    
    }

    int i;
    seg_usage->log_num = tmp_bit_array->len;
    g_free(seg_usage->bitmap);
    seg_usage->bitmap = (char*)g_malloc0((seg_usage->log_num-1)/sizeof(gint)+1);
    //HLOG_DEBUG("size of bitmap:%d",tmp_bit_array->len);
    for(i=0;i<tmp_bit_array->len;i++){
        gint value = g_array_index(tmp_bit_array,gint,i);
        idx = i/sizeof(gint);
        if(value==1){
           //g_message("bitmap idx bit:%d = 1\n",i);
           seg_usage->bitmap[idx] |= 1<<i%sizeof(gint);
           //g_message("bitmap idx %x\n",seg_usage->bitmap[idx]);
       }
    }
    g_array_free(tmp_bit_array,TRUE);
	//HLOG_DEBUG("leave func %s",__func__);
    return 0;
}

