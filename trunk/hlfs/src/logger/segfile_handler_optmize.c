/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "address.h"
#include "storage.h"
#include "icache.h"
#include <errno.h>
#include "clone.h"

int prev_open_rsegfile(struct hlfs_ctrl *ctrl,uint32_t segno){
    //HLOG_DEBUG("enter func %s", __func__);
    struct back_storage * storage =NULL;
    if(NULL == ctrl->last_rsegfile_handler){
        const char segfile_name[SEGMENT_FILE_NAME_MAX];
	 memset(segfile_name,0,SEGMENT_FILE_NAME_MAX);
        build_segfile_name(segno,segfile_name);
        bs_file_t file;   
        if(segno >= ctrl->start_segno){
            storage =  ctrl->storage;
        }else{
            HLOG_DEBUG("get parent storage for segno:%d",segno);
            if(NULL == (storage = get_parent_storage(ctrl->family,segno))){
                g_assert(0);
                return -1;
            }
        }
        file = storage->bs_file_open(storage,segfile_name,BS_READONLY); 
        if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segfile_name);
            g_assert(0);
            return -1;
        }
        ctrl->last_rsegfile_handler = file;
        ctrl->last_rsegfile_offset = ctrl->last_offset;
        ctrl->last_read_segno =segno;
    }else if(ctrl->last_read_segno != segno || (ctrl->last_read_segno == segno && ctrl->last_rsegfile_offset != ctrl->last_offset)){
        HLOG_DEBUG("cur segno:%d is ,last segno no:%d, last rsegfile offset:%d,last offset:%d,start_segno:%d - need close old and open new segfile",
                segno,ctrl->last_read_segno,ctrl->last_rsegfile_offset,ctrl->last_offset,ctrl->start_segno);
        /* close last seg file handler...  */
        if(ctrl->last_read_segno >= ctrl->start_segno){
               storage = ctrl->storage;
        }else{
               HLOG_DEBUG("get parent storage for segno:%d",segno);
               if(NULL == (storage = get_parent_storage(ctrl->family,ctrl->last_read_segno))){
                  g_assert(0);
                  return -1;
              }
        }
        if(0!=storage->bs_file_close(storage,ctrl->last_rsegfile_handler)){
            g_assert(0);
            return -1;
        }	
        /* open cur seg file handler...  */
        const char segfile_name[SEGMENT_FILE_NAME_MAX];
        build_segfile_name(segno,segfile_name);
        if(segno >= ctrl->start_segno){
            storage = ctrl->storage;
        }else{
             HLOG_DEBUG("get parent storage for segno:%d",segno);	
            if(NULL == (storage = get_parent_storage(ctrl->family,segno))){
                g_assert(0);
                return -1;
            }
        }
        bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY); 
        if(file == NULL){
            HLOG_ERROR("can not open segment file %s",segfile_name);
            g_assert(0);
            return -1;
        }
        ctrl->last_rsegfile_handler = file;
        ctrl->last_rsegfile_offset  = ctrl->last_offset;
        ctrl->last_read_segno       = segno;
    }else{
        HLOG_DEBUG("using pre open read file handler");
    }
    //HLOG_DEBUG("leave func %s", __func__);
    return 0;
}
int prev_open_wsegfile(struct hlfs_ctrl *ctrl){
    //HLOG_DEBUG("enter func %s", __func__);
    //HLOG_DEBUG("ctrl last segno %d,offset:%d",ctrl->last_segno,ctrl->last_offset);
    char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(ctrl->last_segno,segfile_name);if( 0 == ctrl->last_offset){
        if(ctrl->last_wsegfile_handler!=NULL){
            if(0!=ctrl->storage->bs_file_close(ctrl->storage,(bs_file_t)ctrl->last_wsegfile_handler)){
                //HLOG_ERROR("close segfile:%d failed",ctrl->last_segno-1);
                g_assert(0);
                return -1;
            }
        }
        HLOG_DEBUG("need make a new segment:%d file",ctrl->last_segno);
        if(NULL == (ctrl->last_wsegfile_handler = (void*)ctrl->storage->bs_file_create(ctrl->storage,segfile_name))){
            //HLOG_ERROR("creat segfile:%d failed,ctrl->last_segno");
            g_assert(0);
            return -1;
        }
    }else{
        if(ctrl->last_wsegfile_handler==NULL){
            /*  open a exist file */
            //HLOG_DEBUG("open a exist file............................");
            if(NULL == (ctrl->last_wsegfile_handler = (void*)ctrl->storage->bs_file_open(ctrl->storage,segfile_name,BS_WRITEABLE))){
                //HLOG_ERROR("open segfile:%d failed,ctrl->last_segno");
                g_assert(0);
                return -1;
            }
        }else{
            HLOG_DEBUG("open a exist segment:%d file",ctrl->last_segno);
        }
    }
    //HLOG_DEBUG("leave func %s", __func__);
    return 0;
}



int read_block_fast(struct hlfs_ctrl *ctrl,uint64_t storage_address,char* block)
{
    //HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    struct back_storage * storage =NULL;
    int write_size = 0;
    uint32_t offset = get_offset(storage_address);
    uint32_t segno = get_segno(storage_address);
    HLOG_DEBUG("offset :%u,segno:%u,last_offset:%u,last_rsegfile_offset:%u",offset,segno,ctrl->last_offset,ctrl->last_rsegfile_offset);
    uint32_t block_size = ctrl->sb.block_size;
    if(0!=prev_open_rsegfile(ctrl,segno)){
        //HLOG_ERROR("can not pre open read segfile:%u",segno);
        g_assert(0);
        return -1; 
    }
    //char * block = (char*)g_malloc0(block_size);
    //if (NULL == block) {
    //    HLOG_ERROR("Allocate Error!");
    //    block = NULL;
    //    goto out;
    //}
    int retry_time = 3;
    do{
        //if(block_size != (write_size = ctrl->storage->bs_file_pread(ctrl->storage,ctrl->last_rsegfile_handler,block,block_size,offset))){
        if(segno >= ctrl->start_segno){
            storage = ctrl->storage;
        }else{
            HLOG_DEBUG("get parent storage for segno:%d",segno);
            if(NULL == (storage = get_parent_storage(ctrl->family,segno))){
                g_assert(0);
                return -1;
            }
        }
        write_size = storage->bs_file_pread(storage,ctrl->last_rsegfile_handler,block,block_size,offset);
        if(write_size!=block_size){
            HLOG_WARN("can not read block from seg:%u#%u :ret :%d",segno,offset,write_size);
            g_usleep(1000);
            retry_time--;
            ret = -1;
        }else{
            HLOG_DEBUG("read block from seg:%u#%u size:%d",segno,offset,write_size);
            ret = 0;
        }
    }while(write_size < block_size && retry_time >0);
out:
        //pre_close_read_segfile(ctrl,segno);
        //HLOG_DEBUG("leave func %s", __func__);
        return ret;
    }



