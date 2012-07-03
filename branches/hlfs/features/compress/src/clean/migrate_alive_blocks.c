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


int migrate_alive_blocks (struct hlfs_ctrl *hctrl,SEG_USAGE_T *seg_usage){
    //HLOG_DEBUG("enter func %s",__func__);
    if(hctrl == NULL || seg_usage == NULL){
		HLOG_ERROR("input params failed");
		return -1;
    }

    //HLOG_DEBUG("hctrl block size:%d",hctrl->sb.block_size);
    int ret = 0;
    const char segfile[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(seg_usage->segno,segfile);
	char *content = NULL;
	uint32_t size;
	if(0!= (ret = file_get_contents(hctrl->storage,segfile,&content,&size))){
			HLOG_ERROR("read segfile:%s failed",segfile);
			return -1;
	}	
    int i= 0,j=0;
    uint32_t offset=0;
    struct log_header *lh = NULL;
    HLOG_DEBUG("log_num: --> %llu", seg_usage->log_num);
    for(i=0;i<seg_usage->log_num;i++){
        lh = (struct log_header*)(content + offset);
        //HLOG_DEBUG("log_idx:%d", i);
        //int idx = (seg_usage->log_num -1 )/8 +1;
        int idx = i / sizeof(gint);
        //HLOG_DEBUG("bitmap:%hhx\n,bit:%d is %hhx", seg_usage->bitmap[idx],i,seg_usage->bitmap[idx]&(i<<(i%8)));
        if(seg_usage->bitmap[idx] & (1<<(i%sizeof(gint)))){
			/* we need check with current inode again */
			int32_t db_start = -1;
			int32_t db_end = -1;
			for(j=0;j<lh->db_num;j++){
				          uint64_t _last_inode_write_timestamp = hctrl->last_write_timestamp;
					   //HLOG_DEBUG("for db:%llu",lh->start_db_no+i);
					   uint64_t db_mine_storage_addr = 0;
					   uint32_t db_mine_storage_addr_offset = offset+LOG_HEADER_LENGTH + j*hctrl->sb.block_size;
					   set_offset(&db_mine_storage_addr,db_mine_storage_addr_offset);
					   set_segno (&db_mine_storage_addr,seg_usage->segno);
					   uint64_t db_cur_storage_addr = get_db_storage_addr_in_inode(hctrl->storage,&hctrl->inode,
			    						                                lh->start_db_no+j,hctrl->sb.block_size);
					   HLOG_DEBUG("db:%llu's mine storage addr:%llu,cur storage addr:%llu",
							   lh->start_db_no+i,db_mine_storage_addr,db_cur_storage_addr);
					   if(db_mine_storage_addr == db_cur_storage_addr){
						  HLOG_DEBUG("this is alive data block");
						  HLOG_DEBUG("copy! ....... ");
            			  HLOG_DEBUG(" log[%d]  need move block:[%d]",i,j);
            			  if(db_start == -1){
						  	 db_start = lh->start_db_no + j;
							 db_end = db_start;
            			  }else{
            			     db_end   = lh->start_db_no + j;
            			  }
					   }
					   if(db_mine_storage_addr != db_cur_storage_addr || j == lh->db_num-1){
					      HLOG_DEBUG("if need ,append it ... ");
					      if(db_start == -1){
						  	  HLOG_DEBUG("do not need to copy this log ....... ");
							  continue;
					      }	
						  #if 0
            			  guint32 BLOCKSIZE = hctrl->sb.block_size;
            			  int expand_size =  (db_end-db_start + 1)*BLOCKSIZE + 
                		  ib_amount(db_start,db_end) * BLOCKSIZE + 
                					LOG_HEADER_LENGTH + 
                					sizeof(struct inode) + 
                					sizeof(struct inode_map_entry);
            			  if (expand_size > hctrl->sb.seg_size) {
                              HLOG_ERROR("write length is beyond the limit length!");
                              return -1;
            			  }
            			  if (hctrl->last_offset + expand_size > hctrl->sb.seg_size) {
               				  hctrl->last_segno++;
                              hctrl->last_offset = 0;
            			  }
            			  HLOG_DEBUG("last segno:%u last offset:%u", hctrl->last_segno,hctrl->last_offset);
					      uint32_t size; 
            			  size = append_log(hctrl,lh->data + (db_start - lh->start_db_no)* hctrl->sb.block_size,(uint32_t) db_start, (uint32_t) db_end);
						  g_assert(size > 0);
						  hctrl->last_offset += size;
						  #else
						   char *db_buff = lh->data + (db_start - lh->start_db_no)* hctrl->sb.block_size;
						   g_mutex_lock  (hctrl->hlfs_access_mutex);
						   if(_last_inode_write_timestamp != hctrl->last_write_timestamp){
						   	  HLOG_INFO("inode maybe has change, for safe reason,redo check again");
							  i--;
							  g_mutex_unlock (hctrl->hlfs_access_mutex);
							  continue;
						   }	
    					   int size = append_log(hctrl,db_buff,db_start,db_end);
    					   g_mutex_unlock (hctrl->hlfs_access_mutex);
    				       if(size < 0){
       						 HLOG_ERROR("append log error");
        					 g_assert(0);
							 return -1;
   					      }
						  #endif 
						  db_start = db_end = -1;
					   }
		    }

        }else{
            HLOG_DEBUG("do not need to copy this log ....... ");
        }

        offset += lh->log_size;
    }
out:
    //HLOG_DEBUG("leave func %s",__func__);
    g_free(content);
    return ret;
}
