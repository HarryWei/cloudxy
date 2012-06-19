/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

/*
 * @author kanghua(kanghua151@msn.com) 
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <glib.h>
#include <string.h>
#include <alloca.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "logger.h"

int hlfs_write(struct hlfs_ctrl *ctrl, char *write_buf, uint32_t write_len, uint64_t pos)
{
    //HLOG_DEBUG("enter func %s", __func__);
    if ((NULL == ctrl) || (NULL == write_buf) || (0 == write_len) || (ctrl->sb.seg_size < write_len)) {
		HLOG_ERROR("Params Error");
		return -1;
    }
	HLOG_INFO("Hlfs Write Req pos:%llu,read_len:%d,last_segno:%d,last_offset:%d,cur_file_len:%d",
    						      pos,
    						      write_len,
    						      ctrl->last_segno,
    						      ctrl->last_offset,
    						      ctrl->inode.length);
    //g_mutex_lock (ctrl->hlfs_access_mutex);
    if(ctrl->rw_inode_flag == 0){
          HLOG_ERROR("your config only allow read request!");
          //g_mutex_unlock (ctrl->hlfs_access_mutex);
          return -1;
    }

    if(ctrl->sb.max_fs_size * 1024 *1024 < pos+write_len){
          HLOG_ERROR("your config only allow write beyond :%d",ctrl->sb.max_fs_size);
          //g_mutex_unlock (ctrl->hlfs_access_mutex);
          return -1;
    }		
    int ret = 0;
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint64_t cur_time;
    uint32_t db_start = pos/BLOCKSIZE;
    uint32_t db_end   = (pos+write_len-1)/BLOCKSIZE;
    int write_blocks_size = (db_end - db_start + 1 )*BLOCKSIZE; 
    char * datablocks = NULL;
    //(char*)g_malloc0(write_blocks_size);
    //if (!datablocks) {
    //        HLOG_ERROR("allocate error!");
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
    //        return -1;
    //    }

    //HLOG_DEBUG("write offset:%llu,write len:%d", pos,write_len);
    if(db_start == db_end && (db_start %BLOCKSIZE != 0 || db_end %BLOCKSIZE != 0) ){
        HLOG_DEBUG("only need to write part in one block:%llu", pos / BLOCKSIZE);
        char *block= (char*)alloca(BLOCKSIZE);
	 g_assert(block!=NULL);
        //g_mutex_lock (ctrl->hlfs_access_mutex);
        ret = load_block_by_addr_fast(ctrl,pos,block);
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
        if( 1==ret ) {
            //HLOG_DEBUG("fail to load block for addr! %llu for not write yet", pos);
            memset(block,0,BLOCKSIZE);
        }else if(-1 == ret){
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }
        datablocks = (char*)g_malloc0(write_blocks_size);
        g_assert(datablocks != NULL);
        memcpy(datablocks,block,pos%BLOCKSIZE);
        memcpy(datablocks + pos%BLOCKSIZE,write_buf,write_len);
        memcpy(datablocks + pos%BLOCKSIZE + write_len, 
               block + pos%BLOCKSIZE + write_len,
               BLOCKSIZE-(pos%BLOCKSIZE+write_len));
        //g_free(block);
        goto write_log;
    }

    if(pos % BLOCKSIZE != 0 ){
        //HLOG_DEBUG("to load first block!");
        char *first_block = (char*)alloca(BLOCKSIZE);
		g_assert(first_block!=NULL);
        //g_mutex_lock (ctrl->hlfs_access_mutex);
        ret = load_block_by_addr_fast(ctrl,pos,first_block);
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(1==ret){
            //HLOG_DEBUG("fail to load first block for not write yet");
            memset(first_block,0,BLOCKSIZE);
        }else if(ret == -1){
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }
        datablocks = (char*)g_malloc0(write_blocks_size);
        g_assert(datablocks != NULL);
        memcpy(datablocks,first_block,BLOCKSIZE);
        memcpy(datablocks+pos%BLOCKSIZE,write_buf,write_len);
        //g_free(first_block);
    }else{
        //HLOG_DEBUG("do not need load first block");
        if( (pos+write_len)%BLOCKSIZE != 0){
             datablocks = (char*)g_malloc0(write_blocks_size);
             g_assert(datablocks != NULL);
             memcpy(datablocks, write_buf, write_len);
        }else{
             HLOG_DEBUG("Do not need alloc any buffer");
             datablocks = write_buf;
             goto write_log;
        }
    }
    if((pos +write_len)%BLOCKSIZE !=0){
        //HLOG_DEBUG("to load last block");
        char *last_block = (char*)alloca(BLOCKSIZE);
        //g_mutex_lock (ctrl->hlfs_access_mutex);
        ret=load_block_by_addr_fast(ctrl, pos + write_len, last_block);
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(1==ret){
            //HLOG_DEBUG("fail to load last block for not write yet");
            memset(last_block,0,BLOCKSIZE);
        }else if(-1==ret){
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            ret = -1;
            goto out;
        }
        memcpy(datablocks+pos%BLOCKSIZE + write_len,
                     last_block+(write_len+pos)%BLOCKSIZE,
                     BLOCKSIZE-(write_len+pos)%BLOCKSIZE);
        //g_free(last_block);
    }
write_log:;
	//HLOG_DEBUG("db_start: %u db_end: %u", db_start, db_end);

	cur_time = get_current_time();
	ctrl->inode.mtime  = cur_time;
	//ctrl->inode.ctime  = cur_time;
	//ctrl->inode.atime  = cur_time;
	//HLOG_DEBUG("get_current_time is %llu", ctrl->inode.mtime);
       //HLOG_DEBUG("length is %llu", ctrl->inode.length);
   
	if(ctrl->cctrl != NULL){
	 	HLOG_DEBUG("we use write back mode !");
        	int ret = cache_insert_blocks(ctrl->cctrl,db_start,(db_end - db_start + 1),datablocks);
        	g_assert(ret == 0);
    }else{
        HLOG_DEBUG("we use write through mode !");
        g_mutex_lock  (ctrl->hlfs_access_mutex);
        ctrl->last_write_timestamp = get_current_time();
        int size = append_log(ctrl,datablocks,db_start,db_end);
        g_mutex_unlock  (ctrl->hlfs_access_mutex);
        if(size < 0){
            HLOG_ERROR("Append Log Error");
            //g_free(datablocks);
            ret =  -1;
            goto out;
        }
	 HLOG_INFO("Append Log Exec Succ . filesize:%llu,last_segno:%u, last_offset:%u,log size:%u,start_dbno:%u,end_dbno:%u",
	 	                                                ctrl->inode.length,
		                                                ctrl->last_segno,
		                                                ctrl->last_offset,
		                                                size,
		                                                db_start,
		                                                db_end)
    }

#if 0
#if 0 /* use async way */
    int size = append_log(ctrl,datablocks,db_start,db_end);
    if(size < 0){
		g_message("fail to append log\n");
		return -1; 
	}
#else
	memset(&(ctrl->write_req), 0, sizeof(struct write_req));
	struct write_req *w_req = &ctrl->write_req;
    //struct write_rsp *w_rsp = &ctrl->write_rsp;
    //struct write_req * w_req = (struct write_req*)g_malloc0(sizeof(struct write_req));
    w_req->req_buf = datablocks;
    w_req->db_start = db_start;
    w_req->db_end = db_end;
    g_async_queue_push(ctrl->write_req_aqueue,(gpointer)w_req);
	if (NULL == w_req) {
		HLOG_DEBUG("g_async_queue_push pushed null data");
	}
	HLOG_DEBUG("ctrl->write_task_run is %d", ctrl->write_task_run);
    HLOG_DEBUG("request pushed to aysn");
    struct write_rsp * w_rsp = (struct write_rsp*)g_async_queue_pop(ctrl->write_rsp_aqueue);
    ret = w_rsp->res;
    int size = w_rsp->size;
    //g_free(w_rsp);
    if(ret!=0){
		HLOG_ERROR("fail to append log");
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
        return -1;
	}
#endif /*use async queue*/
#endif 
out:
    if(datablocks!=NULL && datablocks!=write_buf){
        g_free(datablocks);
    }
	//ctrl->last_access_timestamp = get_current_time();
	//HLOG_DEBUG("leave func %s", __func__);
	if(ctrl->inode.length < (pos + write_len)){ 
		ctrl->inode.length = pos + write_len;
	}
    return write_len;
}
