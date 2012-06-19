/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <string.h>
#include "cache.h"
#include "cache_helper.h"
#include "hlfs_log.h"

int flush_work(gpointer data){
    int ret = 0;
    CACHE_CTRL *cctrl = (CACHE_CTRL*)data;
    GTimeVal expired;
	char* tmp_buf = (char*)g_malloc0(cctrl->block_size * cctrl->flush_once_size);
	g_assert(tmp_buf);
    while (!cctrl->flush_worker_should_exit) {
         HLOG_DEBUG("-- flush worker doing --");
         g_get_current_time(&expired);
         g_time_val_add(&expired, cctrl->flush_interval*1000*1000);
         g_mutex_lock(cctrl->cache_mutex);

         gboolean res = g_cond_timed_wait(cctrl->flush_waken_cond, \
				 cctrl->cache_mutex, &expired);
         g_mutex_unlock(cctrl->cache_mutex); //?
         HLOG_DEBUG(" time wait res for cond is :%d !",res);
         if(cctrl->flush_worker_should_exit){
		 	 HLOG_INFO("-- flush worker should exit --");
             break;
         }
   
         do {
            GSList *continue_blocks = NULL;
            ret = get_continues_blocks(cctrl,&continue_blocks);
            g_assert(ret==0);
            uint32_t blocks_count = g_slist_length(continue_blocks);
            uint32_t buff_len = blocks_count * cctrl->block_size;
            HLOG_DEBUG("--blocks_count:%d,buff_len:%d--",blocks_count,buff_len);
            if(res==TRUE && buff_len==0){
                HLOG_ERROR("Never reach here");
                g_assert(0);
            }
            if(buff_len == 0){
                HLOG_DEBUG("do not need flush now");
                break;
            }
            if(NULL==cctrl->write_callback_func){
                HLOG_WARN("--not given flush callback func--");
                break;
            }
            //char* tmp_buf = g_malloc0(buff_len);
            //g_assert(tmp_buf!=NULL);
            uint32_t start_no;
            uint32_t end_no; 
            int i = 0;
            for (i = 0; i < blocks_count; i++) {
                block_t *block =  g_slist_nth_data(continue_blocks, i);
                if(i==0){
                   start_no = block->block_no;
                }
                if(i==blocks_count-1){
                   end_no = block->block_no;
                }
                memcpy(tmp_buf + i * cctrl->block_size, block->block, cctrl->block_size);
            }
            //HLOG_DEBUG("--tmp_buf:%p",tmp_buf);
            ret = cctrl->write_callback_func(cctrl->write_callback_user_param,tmp_buf,start_no,end_no);
			g_assert(ret >= 0);
            //g_free(tmp_buf);
            if (ret >=0 ) {
                HLOG_DEBUG("--signal write thread--");
                g_mutex_lock(cctrl->cache_mutex);
                __free_from_cache(cctrl, continue_blocks);
                //g_cond_broadcast(cctrl->writer_waken_cond);
                g_cond_signal(cctrl->writer_waken_cond);
                g_mutex_unlock(cctrl->cache_mutex);
                g_slist_free(continue_blocks);
                //HLOG_DEBUG("--return blocks to cache over--");
            }
         } while (get_cache_free_size(cctrl) < cctrl->flush_trigger_level * cctrl->cache_size / 100 || (res == 0 && get_cache_free_size(cctrl) !=0));
    }
    HLOG_INFO("--flush worker exit--");
    return 0;
}
