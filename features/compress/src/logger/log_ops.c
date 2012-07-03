/*
 * @author kanghua(kanghua151@msn.com) 
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
#include "snappy-c.h"

/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "storage.h"
#include "icache.h"





static void dump_iblock(char *iblock){
    uint64_t *_iblock = (uint64_t*)iblock;
    int i=0;
    for(i=0;i<1024;i++){
        HLOG_DEBUG("i:%d,addr:%llu",i,*_iblock);
        _iblock++;
    }
}



int compress_dblocks(char *db_buff,uint32_t db_num,uint32_t block_size,char *dzb_buff,uint32_t *real_compressed_size){
	int i=0;
	char *cur_db=db_buff;
	int offset=0;
	for(i=0;i<db_num;i++){
		cur_db = db_buff+i*block_size;
		size_t output_length = 0;
		g_assert(snappy_compress(cur_db,block_size,dzb_buff + offset + sizeof(uint32_t),&output_length)== SNAPPY_OK);
		*(uint32_t*)(dzb_buff+offset) = output_length;
		offset += output_length + sizeof(uint32_t); 
	}
	*real_compressed_size = offset;
	return 0;
}

int get_zblock_size(char *dzb){
	uint32_t size = *(uint32_t*)dzb;
	return size; 
}



static int update_inode_index(struct inode *inode, struct log_header * log,uint32_t last_segno,uint32_t last_offset,uint32_t block_size){
    //HLOG_DEBUG("enter func %s", __func__);
    if(NULL == inode || NULL == log){
        //HLOG_DEBUG("params is error");
        return -1;
    }
	uint32_t BLOCKSIZE = block_size;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
    uint32_t db_offset		= LOG_HEADER_LENGTH;
    uint32_t start_db	    = log->start_db_no;
    uint32_t db_num	        = log->db_num;
    uint32_t end_db	        = start_db + db_num - 1;
	uint32_t ib_offset = 0;
	if(log->cflag == 1){
	   int i;
	   for(i=0;i< db_num;i++){
	   	   db_offset += get_zblock_size((char*)log + db_offset) + sizeof(uint32_t);
	   }
	   ib_offset = db_offset;
	   db_offset = LOG_HEADER_LENGTH;
	}else{
	   ib_offset      = db_offset + db_num * BLOCKSIZE ;
	}
  

    guint32  db_cur_no = 0;
    for(db_cur_no = start_db; db_cur_no <= end_db; db_cur_no++){
        if (is_db_in_level1_index_range(db_cur_no)){
            HLOG_DEBUG(" is level1 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            /*  write db to log buff  */
            int _idx = db_cur_no % 12;
            //HLOG_DEBUG(" idx:%u",_idx);
            set_segno (&inode->blocks[_idx],last_segno);
            set_offset (&inode->blocks[_idx],last_offset + db_offset);
		    HLOG_DEBUG("-----dbno:%d,idx:%d, blocks:%lld,db_offset:%d",db_cur_no,_idx,inode->blocks[_idx],db_offset); 
			if(log->cflag==0){
            	db_offset += BLOCKSIZE;
			}else{
				db_offset += get_zblock_size((char*)log + db_offset) + sizeof(uint32_t);
			}
        }else if (is_db_in_level2_index_range(db_cur_no)){
            if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == end_db){ 
                HLOG_DEBUG(" is level2 -- db_cur_no:%d ib_offset:%d",db_cur_no,ib_offset);
                set_segno  (&inode->iblock, last_segno);
                set_offset (&inode->iblock, last_offset + ib_offset);
				//HLOG_DEBUG("-----iblock : %lld",inode->iblock);                
		        HLOG_DEBUG("-----dbno:%d,iblock:%llu,db_offset:%d",db_cur_no,inode->iblock,ib_offset);                
                //dump_iblock((char*)log+ib_offset);
                //ib_offset +=BLOCKSIZE;
				if(log->cflag==0){
            		ib_offset += BLOCKSIZE;
				}else{
					ib_offset += get_zblock_size((char*)log + ib_offset) + sizeof(uint32_t);
				}
            }
        }else if (is_db_in_level3_index_range(db_cur_no)){
            HLOG_DEBUG(" enter level3 ... ib_offset:%d",ib_offset);
			if((db_cur_no -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == end_db){
				  ib_offset +=BLOCKSIZE;
			}
		     
            if((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == end_db){
                set_segno (&inode->doubly_iblock,last_segno);
                set_offset(&inode->doubly_iblock,last_offset + ib_offset);
                //dump_iblock((char*)log+ib_offset);
                //ib_offset +=BLOCKSIZE;
				if(log->cflag==0){
            		ib_offset += BLOCKSIZE;
				}else{
					ib_offset += get_zblock_size((char*)log + ib_offset) + sizeof(uint32_t);
				}
            }
        }else if (is_db_in_level4_index_range(db_cur_no)){
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == end_db){
				HLOG_DEBUG(" save ib3 ? ");
                ib_offset +=BLOCKSIZE;
            }
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || db_cur_no == end_db){
				HLOG_DEBUG(" save ib2 ? ");
                ib_offset +=BLOCKSIZE;
            }
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == end_db){
                set_segno (&inode->triply_iblock,last_segno);
                set_offset(&inode->triply_iblock,last_offset + ib_offset);
                //ib_offset +=BLOCKSIZE;
				if(log->cflag==0){
            		ib_offset += BLOCKSIZE;
				}else{
					ib_offset += get_zblock_size((char*)log + ib_offset) + sizeof(uint32_t);
				}
            }
        }
    }
	//memcpy(log + ib_offset,inode,sizeof(struct inode));
	//g_assert(ib_offset == log->size - sizeof(struct inode_map_entry));
    return 0;		 
}

static int update_icache(struct icache_ctrl *icctrl,char *log_iblock_buf,uint32_t db_start_no,uint32_t db_num){
    //HLOG_DEBUG("enter func %s", __func__);
    if(NULL == icctrl){
        HLOG_ERROR("icctrl is null");
        return -1;
    }
    int ret;
    guint32 BLOCKSIZE = icctrl->iblock_size;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
    int offset=0;
    int i;
    for(i=db_start_no;i<db_start_no+db_num;i++){
        if(is_db_in_level1_index_range(i)){
        }else if(is_db_in_level2_index_range(i)){
            if((i - 12 + 1) % IB_ENTRY_NUM == 0 || i == db_start_no+db_num-1){
                int ibno = get_layer1_ibno(i);
                //HLOG_DEBUG("dbno:%d,ibno:%d,offset:%d,log_iblock:%p",i,ibno,offset,log_iblock_buf);
                int  _idx = (i-12)%IB_ENTRY_NUM;
                uint64_t * ib=(uint64_t*)(log_iblock_buf + offset);
                uint64_t storage_address = *(ib+_idx);
                //HLOG_DEBUG("dbno:%d,ibno:%d,idx:%d,storage_addr:%llu",i,ibno,_idx,storage_address);
                g_assert(ibno>=0);

                ret = icache_insert_iblock(icctrl,ibno,(char*)log_iblock_buf + offset);
                g_assert(ret==0);
                offset += BLOCKSIZE;
            }   
        }else if(is_db_in_level3_index_range(i)){
            if((i -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || i == db_start_no+db_num-1){
                int ibno2 = get_layer2_ibno(i);
                //HLOG_DEBUG("dbno:%d,ibno2:%d",i,ibno2);
                g_assert(ibno2>0);	
                ret = icache_insert_iblock(icctrl,ibno2,(char*)log_iblock_buf + offset);
                g_assert(ret==0);
                offset += BLOCKSIZE;
            }
            if((i - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 ||  i == db_start_no+db_num-1){
                int ibno1 = get_layer1_ibno(i);
                //HLOG_DEBUG("ibno1:%d",ibno1);
                g_assert(ibno1>0);
                ret = icache_insert_iblock(icctrl,ibno1,(char*)log_iblock_buf + offset);
                g_assert(ret==0);	
                offset += BLOCKSIZE;
            }
        }else if(is_db_in_level4_index_range(i)){

            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 ||i == db_start_no+db_num - 1){

                int ibno3 = get_layer3_ibno(i);
                //HLOG_DEBUG("dbno:%d,ibno3:%d",i,ibno3);
                g_assert(ibno3>0);		
                ret = icache_insert_iblock(icctrl,ibno3,(char*)log_iblock_buf + offset);
                g_assert(ret==0);
                offset += BLOCKSIZE;
            }

            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || i == db_start_no+db_num -1){

                int ibno2 = get_layer2_ibno(i);
                //HLOG_DEBUG("dbno:%d,ibno2:%d",i,ibno2);
                g_assert(ibno2>0);		
                ret = icache_insert_iblock(icctrl,ibno2,(char*)log_iblock_buf + offset);
                g_assert(ret==0);
                offset += BLOCKSIZE;
            }

            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 ||i == db_start_no+db_num -1){

                int ibno1 = get_layer1_ibno(i);
                //HLOG_DEBUG("dbno:%d,ibno3:%d",i,ibno1);
                g_assert(ibno1>0);
                ret = icache_insert_iblock(icctrl,ibno1,(char*)log_iblock_buf + offset);
                g_assert(ret==0);
                offset += BLOCKSIZE;
            }
        }else{
            g_assert(0);
        }
    }	 	
    //HLOG_DEBUG("exit func %s", __func__);
    return 0;
}


//static bs_file_t g_cur_write_file = NULL;
static int dump_log(struct hlfs_ctrl *ctrl,struct log_header *log){
    //HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    if( 0 != prev_open_wsegfile(ctrl)){
        HLOG_ERROR("fail do pre open segfile");
        ret -1;
    }
    int size = ctrl->storage->bs_file_append(ctrl->storage,(bs_file_t)ctrl->last_wsegfile_handler,(char*)log,log->log_size);
    HLOG_DEBUG("write to filewrite size %d  expect size %d",size,log->log_size);
    if(size != log->log_size){
        HLOG_ERROR("write to seg:%d failed, write size %d  expect size %d # ",
                ctrl->last_segno,size,log->log_size,ctrl->last_wsegfile_handler);
        return -1;
    } 
    if (0!=ctrl->storage->bs_file_flush(ctrl->storage,(bs_file_t)ctrl->last_wsegfile_handler)){
        HLOG_ERROR("storage flush failed");
        return -1;
    }else{
        if(NULL != ctrl->icache){
			#if 0
            guint32 db_data_len = log->db_num * ctrl->sb.block_size;
            guint32 ib_offset   = db_data_len + LOG_HEADER_LENGTH;
            HLOG_DEBUG("-- db_num:%d,ib_offset:%d,log:%p",log->db_num,ib_offset,(char*)log + ib_offset);
            ret = update_icache(ctrl->icache,(char*)log + ib_offset,log->start_db_no,log->db_num);
            g_assert(ret==0);
			#endif
        }
        //g_mutex_lock (ctrl->hlfs_access_mutex);
        //HLOG_DEBUG("last offset ................... %d",ctrl->last_offset);
        ret = update_inode_index(&ctrl->inode,log,ctrl->last_segno,ctrl->last_offset,ctrl->sb.block_size);
        g_assert(ret == 0);
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
    }		 	
    //HLOG_DEBUG("leave func %s", __func__);
    return size;
}



int __append_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end,uint32_t no_compressed){
    //HLOG_DEBUG("entry func: %s",__func__);
    gboolean ib1_need_load  = TRUE;
    gboolean ib2_need_load  = TRUE;
    gboolean ib3_need_load  = TRUE;
	guint32 BLOCKSIZE = ctrl->sb.block_size;
	uint64_t*  ib1 = (uint64_t*)alloca(BLOCKSIZE);
	uint64_t*  ib2 = (uint64_t*)alloca(BLOCKSIZE);
	uint64_t*  ib3 = (uint64_t*)alloca(BLOCKSIZE);
	memset(ib1,0,sizeof(BLOCKSIZE));
	memset(ib2,0,sizeof(BLOCKSIZE));
	memset(ib3,0,sizeof(BLOCKSIZE));
	g_assert(NULL != ib1);
	g_assert(NULL != ib2);
	g_assert(NULL != ib3);
    guint32  ib_cur_offset = 0;
    guint32  db_offset = 0;
    guint32  ib_offset = 0;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);

	gboolean _is_compressed= (no_compressed == 0) ? FALSE:ctrl->is_compressed;
	char* log_buff = NULL;
	if(!_is_compressed){
	    uint32_t db_data_len = (db_end-db_start + 1) * BLOCKSIZE;
	    uint32_t ib_data_len = ib_amount(db_start, db_end) * BLOCKSIZE;
	    log_buff = (char*)g_malloc0(db_data_len + ib_data_len + 
	            sizeof(struct inode) + sizeof(struct inode_map_entry) + 
	            sizeof(struct log_header));
	    if (!log_buff) {
		    HLOG_ERROR("allocate error!");
	        g_assert(0);
		    return -1;
	    }
		memcpy(log_buff + LOG_HEADER_LENGTH,db_buff,db_data_len);
		ib_offset =  db_data_len + LOG_HEADER_LENGTH;
	}else{
	    HLOG_DEBUG("COMPRESSED: need compressed block !!!!");
		uint32_t max_compressed_size = snappy_max_compressed_length(BLOCKSIZE);
		HLOG_DEBUG("COMPRESSED: max_compressed_size :%d",max_compressed_size);
		log_buff = (char*)g_malloc0((max_compressed_size + sizeof(uint32_t)) * (db_end-db_start + 1) + 
									(max_compressed_size + sizeof(uint32_t)) * ib_amount(db_start, db_end) + 
	            sizeof(struct inode) + sizeof(struct inode_map_entry) + 
	            sizeof(struct log_header));
		g_assert(log_buff!=NULL);
		char *compressed_db_buff = log_buff + LOG_HEADER_LENGTH;
		uint32_t real_compressed_size = 0;
		int ret = compress_dblocks(db_buff,(db_end-db_start + 1),BLOCKSIZE,compressed_db_buff,&real_compressed_size);
		HLOG_DEBUG("COMPRESSED: real_compressed_size :%d",real_compressed_size);
		g_assert(ret == 0);
		ib_offset = LOG_HEADER_LENGTH + real_compressed_size;
	}
	//memcpy(log_buff + LOG_HEADER_LENGTH,db_buff,size);
    guint32 db_cur_no = 0;
    guint32 i=0;
	char * cur_log_buff_ptr = NULL;
    //HLOG_DEBUG(" db_data_len:%d ib_data_len:%d BLOCKSIZE:%d",db_data_len,ib_data_len,BLOCKSIZE);
    for(db_cur_no = db_start,i=0; db_cur_no <= db_end; db_cur_no++,i++){  
        /* 
         * char * cur_block_ptr = (char *) (db_buff + i * BLOCKSIZE);  
         * db_offset = LOG_HEADER_LENGTH + i*BLOCKSIZE;
         * char * cur_log_buff_ptr = log_buff + db_offset;
         */
        cur_log_buff_ptr = log_buff + LOG_HEADER_LENGTH + db_offset;
        HLOG_DEBUG(" db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
        if(is_db_in_level1_index_range(db_cur_no)){
            HLOG_DEBUG(" is level1 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            /*  write db to log buff  */
            int _idx = db_cur_no % 12;
            //HLOG_DEBUG(" idx:%u",_idx);
	     #if 0
            set_segno (&ctrl->inode.blocks[_idx],ctrl->last_segno);
            set_offset(&ctrl->inode.blocks[_idx],ctrl->last_offset + db_offset);
	     #endif
           /*memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);*/
		   if(_is_compressed){
		   		db_offset += get_zblock_size(cur_log_buff_ptr)  + sizeof(uint32_t);
		   }else{
		    	db_offset += BLOCKSIZE;
		   }
        }else if(is_db_in_level2_index_range(db_cur_no)){
           HLOG_DEBUG("is level2 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
           if(TRUE == ib1_need_load && ctrl->inode.iblock == 0){
                    HLOG_DEBUG("---------------set iblock zero---------------------");
                    memset(ib1,0,BLOCKSIZE);
					ib1_need_load = FALSE;
           }else if( TRUE == ib1_need_load && ctrl->inode.iblock != 0){
                    if(0>read_layer1_iblock(ctrl,db_cur_no,ib1)){ 	
                        if (0 != read_block_fast(ctrl,ctrl->inode.iblock,ib1)){
                            HLOG_ERROR("read block error!");
                            g_assert(0);
                            return -1;
                        }
			            /*write_layer1_iblock(ctrl,db_cur_no,ib1);*/
                    }
                    ib1_need_load = FALSE;
            }
            int  _idx = (db_cur_no - 12)%IB_ENTRY_NUM;
            //uint64_t storage_address = *(_bi + _idx);
            set_segno ((ib1+_idx),ctrl->last_segno);
            set_offset ((ib1+_idx),ctrl->last_offset + db_offset);
            //HLOG_DEBUG("-- cur_dbno:%d,idx:%d,storage address:%llu",db_cur_no,_idx,*(ib1+_idx));
            /*memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);*/
            if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end ){
                HLOG_DEBUG("set iblock - segno:%u ???",ctrl->last_segno);
		  		#if 0
                set_segno(&ctrl->inode.iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.iblock,ctrl->last_offset + ib_offset);
		  		#endif
				char *ib1_buff = NULL;
				uint32_t ib1_buff_size = 0;
                if(_is_compressed){
				  HLOG_DEBUG("COMPRESSED: need compressed for iblock");
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
                  ib1_buff = (char*)alloca(output_length);
    			  g_assert(snappy_compress(ib1,BLOCKSIZE,ib1_buff,&output_length)== SNAPPY_OK);
				  ib1_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib1_buff_size %d",ib1_buff_size);
				  memcpy((char*)log_buff + ib_offset + sizeof(uint32_t),(char*)ib1_buff,ib1_buff_size);  
                  *(uint32_t*)((char*)log_buff+ib_offset) = ib1_buff_size;
                  ib_offset += ib1_buff_size + sizeof(uint32_t);
                }else{
                  ib1_buff = ib1;
				  ib1_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset ,(char*)ib1_buff,ib1_buff_size);  
                  ib_offset += ib1_buff_size;
                }
		
                ib1_need_load=TRUE;
                //dump_iblock(ib1);
                #if 1
                write_layer1_iblock(ctrl,db_cur_no,ib1);
				#endif 
	            memset(ib1,0,sizeof(BLOCKSIZE));
            }
		  	if(_is_compressed){
		   		db_offset += get_zblock_size(cur_log_buff_ptr)  + sizeof(uint32_t);
		    }else{
		    	db_offset += BLOCKSIZE;
		    }
        }else if(is_db_in_level3_index_range(db_cur_no)){
            HLOG_DEBUG(" is level3 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
                if(TRUE == ib1_need_load && ctrl->inode.doubly_iblock == 0){
					HLOG_DEBUG("set ib1 zero");
                    memset(ib1,0,BLOCKSIZE);
					ib1_need_load = FALSE;
                }else if( TRUE== ib1_need_load && ctrl->inode.doubly_iblock !=0){
                    if(0>read_layer1_iblock(ctrl,db_cur_no,ib1)){ 	
                        if(0 != read_block_fast(ctrl,ctrl->inode.doubly_iblock,ib1)){
                            HLOG_ERROR("allocate error!");
                            g_assert(0);
                            return -1;
                        }
			   		/*  write_layer1_iblock(ctrl,db_cur_no,ib1); */
                    }
                    ib1_need_load = FALSE;
                }
       
            //dump_iblock(ib1);
            int _idx   = (db_cur_no - 12 - IB_ENTRY_NUM) / IB_ENTRY_NUM;
            //HLOG_DEBUG("cur_dbno:%u,idx:%u,*(ib1+_idx):%llu",db_cur_no,_idx,*(ib1+_idx));
            //uint64_t *_ib2 = NULL; 
            if(TRUE == ib2_need_load && *(ib1+_idx) == 0 ){
					HLOG_DEBUG("set ib2 zero");
                    memset(ib2,0,BLOCKSIZE);
					ib2_need_load = FALSE;
            }else if( TRUE == ib2_need_load && *(ib1+_idx) != 0){
                    if(0>read_layer2_iblock(ctrl,db_cur_no,ib2)){ 	
                        if ( 0!=read_block_fast(ctrl,*(ib1+_idx),ib2)){
                            HLOG_ERROR("allocate error!");
                            g_assert(0);
                            return -1;
                        }
			   		/*  write_layer2_iblock(ctrl,db_cur_no,ib2);*/
                    }
                    ib2_need_load = FALSE;
            }
         

            //dump_iblock(ib2);
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM)%IB_ENTRY_NUM;
            
            set_segno ((ib2+_idx2),ctrl->last_segno);
            set_offset((ib2+_idx2),ctrl->last_offset + db_offset);
            //memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
            //HLOG_DEBUG("cur_dbno:%u,_idx2:%u,*(ib2+_idx2):%llu",db_cur_no,_idx2,*(ib2+_idx2));


            if((db_cur_no -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                //HLOG_DEBUG(" save ib2");
                set_segno ((ib1+_idx),ctrl->last_segno);
                set_offset((ib1+_idx),ctrl->last_offset + ib_offset);
				char *ib2_buff = NULL;
				uint32_t ib2_buff_size = 0;
				if(_is_compressed){
				  HLOG_DEBUG("COMPRESSED: for iblock");
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
				  ib2_buff = (char*)alloca(output_length);
				  g_assert(snappy_compress(ib2,BLOCKSIZE,ib2_buff,&output_length)== SNAPPY_OK);
				  ib2_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib2_buff_size %d",ib2_buff_size);
				  memcpy((char*)log_buff + ib_offset +sizeof(uint32_t),(char*)ib2_buff,ib2_buff_size);  
				  //ib_offset +=BLOCKSIZE;
				  *(uint32_t*)((char*)log_buff+ib_offset) = ib2_buff_size;
				  ib_offset += ib2_buff_size + sizeof(uint32_t);
				}else{
				  ib2_buff = ib2;
				  ib2_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset,(char*)ib2_buff,ib2_buff_size);  
				  ib_offset += ib2_buff_size;
				}
		
				ib2_need_load=TRUE;
				//dump_iblock(ib1);
				#if 1
                write_layer2_iblock(ctrl,db_cur_no,ib2);
				#endif 
				memset(ib2,0,sizeof(BLOCKSIZE));
            }

            if((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
                //HLOG_DEBUG(" save ib1");
		  #if 0
                set_segno (&ctrl->inode.doubly_iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.doubly_iblock,ctrl->last_offset + ib_offset);
		  #endif
                //memcpy((char*)log_buff + ib_offset,(char*)ib1,BLOCKSIZE);
				//HLOG_DEBUG("ib1 address:%p",log_buff+ib_offset);
                //ib1_need_load=TRUE;
	            //memset(ib1,0,sizeof(BLOCKSIZE));
                //ib_offset +=BLOCKSIZE;
                char *ib1_buff = NULL;
				uint32_t ib1_buff_size = 0;
                if(_is_compressed){
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
                  ib1_buff = (char*)alloca(output_length);
    			  g_assert(snappy_compress(ib1,BLOCKSIZE,ib1_buff,&output_length)== SNAPPY_OK);
				  ib1_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib1_buff_size %d",ib1_buff_size);
				  memcpy((char*)log_buff + ib_offset + sizeof(uint32_t),(char*)ib1_buff,ib1_buff_size);  
				  *(uint32_t*)((char*)log_buff+ib_offset) = ib1_buff_size;
				  ib_offset += ib1_buff_size;
                }else{
                  ib1_buff = ib1;
				  ib1_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset,(char*)ib1_buff,ib1_buff_size);  
                  //ib_offset +=BLOCKSIZE;
                  ib_offset += ib1_buff_size;
                }
			  
                ib1_need_load=TRUE;
                //dump_iblock(ib1);
                #if 1
                write_layer1_iblock(ctrl,db_cur_no,ib1);
				#endif 
	            memset(ib1,0,sizeof(BLOCKSIZE));
            }
            if(_is_compressed){
		   		db_offset += get_zblock_size(cur_log_buff_ptr)  + sizeof(uint32_t);
		    }else{
		    	db_offset += BLOCKSIZE;
		    }
        }else if(is_db_in_level4_index_range(db_cur_no)){
            HLOG_DEBUG(" is level4 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
     
                if(TRUE == ib1_need_load && ctrl->inode.triply_iblock == 0){
                     memset(ib1,0,BLOCKSIZE);
					 ib1_need_load = FALSE;
                }else if(TRUE == ib1_need_load && ctrl->inode.triply_iblock !=0 ){
                    if(0>read_layer1_iblock(ctrl,db_cur_no,&ib1)){ 	
                        if ( 0 != read_block_fast(ctrl,ctrl->inode.triply_iblock,ib1)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
			        /*  write_layer1_iblock(ctrl,db_cur_no,ib1);*/
                    }
					ib1_need_load = FALSE;
                }
 
            int _idx   = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM * IB_ENTRY_NUM);
                if(TRUE == ib2_need_load && *(ib1+_idx) == 0 ){
                     memset(ib2,0,BLOCKSIZE);
					 ib2_need_load = FALSE;
                }else if( TRUE == ib2_need_load && *(ib1 + _idx) != 0){
                    if(0>read_layer2_iblock(ctrl,db_cur_no,ib2)){ 	
                        if (0 != read_block_fast(ctrl,*(ib1+_idx),ib2)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
			   		    /* write_layer2_iblock(ctrl,db_cur_no,ib2);*/
                    }
					ib2_need_load = FALSE;
                }
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / IB_ENTRY_NUM % IB_ENTRY_NUM;
                if(TRUE == ib3_need_load && *(ib2+_idx2) == 0 ){
                     memset(ib3,0,BLOCKSIZE);
					 ib3_need_load = FALSE;
                }else if( TRUE == ib3_need_load && *(ib2 + _idx2) !=0 ){
                    if(0>read_layer3_iblock(ctrl,db_cur_no,ib3)){ 	
                        if ( 0!= read_block_fast(ctrl,*(ib2+_idx2),ib3)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
                    }
		      		/* write_layer3_iblock(ctrl,db_cur_no,ib3);*/
                    ib3_need_load = FALSE;
                }
            int _idx3  = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
            set_segno ((ib3+_idx3),ctrl->last_segno);
            set_offset((ib3+_idx3),ctrl->last_offset + db_offset);
            //memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                set_segno ((ib2+_idx2),ctrl->last_segno);
                set_offset ((ib2+_idx2),ctrl->last_offset + ib_offset);
                //memcpy((char*)log_buff + ib_offset,(char*)ib3,BLOCKSIZE);
                //ib_offset +=BLOCKSIZE;
                //ib3_need_load= TRUE;
	            //memset(ib3,0,sizeof(BLOCKSIZE));
	            char *ib3_buff = NULL;
				uint32_t ib3_buff_size = 0;
				if(_is_compressed){
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
				  ib3_buff = (char*)alloca(output_length);
				  g_assert(snappy_compress(ib2,BLOCKSIZE,ib3_buff,&output_length)== SNAPPY_OK);
				  ib3_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib3_buff_size %d",ib3_buff_size);
				  memcpy((char*)log_buff + ib_offset + sizeof(uint32_t),(char*)ib3_buff,ib3_buff_size);  
				  //ib_offset +=BLOCKSIZE;
				  *(uint32_t*)((char*)log_buff+ib_offset) = ib3_buff_size;
				  ib_offset += ib3_buff_size;
				}else{
				  ib3_buff = ib3;
				  ib3_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset,(char*)ib3_buff,ib3_buff_size);  
				  //ib_offset +=BLOCKSIZE;
				  ib_offset += ib3_buff_size;
				}
				
				ib3_need_load=TRUE;
				#if 1
                write_layer3_iblock(ctrl,db_cur_no,ib3);
				#endif 
				memset(ib3,0,sizeof(BLOCKSIZE));
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || db_cur_no == db_end){
                set_segno ((ib1+_idx),ctrl->last_segno);
                set_offset ((ib1+_idx),ctrl->last_offset + ib_offset);
                //memcpy((char*)log_buff + ib_offset,(char*)ib2,BLOCKSIZE);
                //ib_offset +=BLOCKSIZE;
				//ib2_need_load = TRUE;
	            //memset(ib2,0,sizeof(BLOCKSIZE));
	            char *ib2_buff = NULL;
				uint32_t ib2_buff_size = 0;
				if(_is_compressed){
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
				  ib2_buff = (char*)alloca(output_length);
				  g_assert(snappy_compress(ib2,BLOCKSIZE,ib2_buff,&output_length)== SNAPPY_OK);
				  ib2_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib2_buff_size %d",ib2_buff_size);
				  memcpy((char*)log_buff + ib_offset + sizeof(uint32_t),(char*)ib2_buff,ib2_buff_size);  
				  *(uint32_t*)((char*)log_buff+ib_offset) = ib2_buff_size;
				  //ib_offset +=BLOCKSIZE;
				  ib_offset += ib2_buff_size;
				}else{
				  ib2_buff = ib2;
				  ib2_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset,(char*)ib2_buff,ib2_buff_size);  
				  //ib_offset +=BLOCKSIZE;
				  ib_offset += ib2_buff_size;
				}
				
				ib2_need_load=TRUE;
				//dump_iblock(ib1);
				#if 1
                write_layer2_iblock(ctrl,db_cur_no,ib2);
				#endif 
				memset(ib2,0,sizeof(BLOCKSIZE));
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
		  		#if 0
                set_segno (&ctrl->inode.triply_iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.triply_iblock,ctrl->last_offset + ib_offset);
		  		#endif
                //memcpy((char*)log_buff + ib_offset,(char*)ib1,BLOCKSIZE);
                //ib_offset +=BLOCKSIZE;
				//ib1_need_load = TRUE;
	            //memset(ib1,0,sizeof(BLOCKSIZE));
	            char *ib1_buff = NULL;
				uint32_t ib1_buff_size = 0;
				if(_is_compressed){
				  size_t output_length = snappy_max_compressed_length(BLOCKSIZE);
				  ib1_buff = (char*)alloca(output_length);
				  g_assert(snappy_compress(ib2,BLOCKSIZE,ib1_buff,&output_length)== SNAPPY_OK);
				  ib1_buff_size = output_length;
				  HLOG_DEBUG("COMPRESSED: after compress ib1_buff_size %d",ib1_buff_size);
				  memcpy((char*)log_buff + ib_offset + sizeof(uint32_t),(char*)ib1_buff,ib1_buff_size);  
				  //ib_offset +=BLOCKSIZE;
				  ib_offset += ib1_buff_size;
				}else{
				  ib1_buff = ib1;
				  ib1_buff_size = BLOCKSIZE;
				  memcpy((char*)log_buff + ib_offset,(char*)ib1_buff,ib1_buff_size);
				  *(uint32_t*)((char*)log_buff+ib_offset) = ib1_buff_size;
				  //ib_offset +=BLOCKSIZE;
				  ib_offset += ib1_buff_size;
				}
				
				ib1_need_load=TRUE;
				//dump_iblock(ib1);
				#if 1
                write_layer1_iblock(ctrl,db_cur_no,ib1);
				#endif 
				memset(ib1,0,sizeof(BLOCKSIZE));
            }
 		    if(_is_compressed){
		   		db_offset += get_zblock_size(cur_log_buff_ptr)  + sizeof(uint32_t);
		    }else{
		    	db_offset += BLOCKSIZE;
		    }
        }else{
            /* over limit size  */
            HLOG_ERROR("offset is out of limit size(8T)!!!");
            return -1;   
        }
    }
__inode_create:;
               HLOG_DEBUG("to update inode ...");
               int offset = ib_offset;
               //HLOG_DEBUG("to update inode map entry ...");
               HLOG_DEBUG("last offset:%u , last segno:%u log head len:%d iboffset:%d", ctrl->last_offset,ctrl->last_segno,LOG_HEADER_LENGTH,ib_offset);
               ctrl->imap_entry.inode_no = HLFS_INODE_NO; 
		       #if 1
               set_segno (&ctrl->imap_entry.inode_addr,ctrl->last_segno);     
               set_offset(&ctrl->imap_entry.inode_addr,ctrl ->last_offset + offset);    
               HLOG_DEBUG("inode address's offset %llu , give it %u",ctrl->imap_entry.inode_addr,ctrl->last_offset + offset);
	           #endif 
               memcpy((char*)log_buff +  offset + sizeof(struct inode),&ctrl->imap_entry,sizeof(struct inode_map_entry));
               HLOG_DEBUG("to fill log header ...");
               struct log_header * lh = (struct log_header *)log_buff;
               //lh->version = 0;
               //lh->header_checksum = 0;
               //lh->data_checksum = 0;
               lh->log_size = offset + sizeof(struct inode) + sizeof(struct inode_map_entry);
               //lh->ctime = get_current_time();
               lh->start_db_no = db_start;
               //g_assert(db_data_len%BLOCKSIZE == 0);
               //g_assert((ib_offset-db_data_len-LOG_HEADER_LENGTH)%BLOCKSIZE == 0);
               lh->db_num = (db_end-db_start + 1);
               lh->ib_num = ib_amount(db_start, db_end);
 			   #if 1  /* modify inode in log's ib,but not yet modify in ctrl */
			   struct inode _inode;
			   memcpy((char*)&_inode,(char*)&ctrl->inode,sizeof(struct inode));
			   update_inode_index(&_inode,lh,ctrl->last_segno,ctrl->last_offset,ctrl->sb.block_size);
               memcpy((char*)log_buff + offset,(char*)&_inode,sizeof(struct inode));
			   #endif
			   if(_is_compressed == TRUE){
			   	  HLOG_DEBUG("COMPRESSED: set cflag to 1");
			   	  lh->cflag = 1;
			   }else{
			      lh->cflag = 0;
			   }
               HLOG_DEBUG("log size:%d,log header:%d,inode:%d,inode map:%d,dbnum:%d,ibnum:%d",lh->log_size,sizeof(struct log_header),sizeof(struct inode),sizeof(struct inode_map_entry),lh->db_num,lh->ib_num); 
               if(0 >= dump_log(ctrl,lh)){
                   HLOG_ERROR("log dump failed");
                   g_assert(0);
               }
               int size = lh->log_size;
               HLOG_DEBUG("return log size :%d",lh->log_size);
               g_free(log_buff);
			   
               //HLOG_DEBUG("leave func %s", __func__);
               return size;
}

int append_log(struct hlfs_ctrl *hctrl,const char *db_buff,uint32_t db_start,uint32_t db_end,uint32_t no_compressed){
	guint32 BLOCKSIZE = hctrl->sb.block_size;
	int expand_size =	(db_end-db_start + 1)*BLOCKSIZE + ib_amount(db_start,db_end) * BLOCKSIZE + 
			 														LOG_HEADER_LENGTH + 
			  														sizeof(struct inode) + 
			  														sizeof(struct inode_map_entry);
	if (expand_size > hctrl->sb.seg_size) {
		HLOG_ERROR("write length is beyond the limit length!");
		return -1;
	}
	//TODO maybe revise for compressed reason?
	if (hctrl->last_offset + expand_size > hctrl->sb.seg_size) {
		hctrl->last_segno++;
		hctrl->last_offset = 0;
	}
	//HLOG_DEBUG("last segno:%u last offset:%u", hctrl->last_segno,hctrl->last_offset);
	uint32_t size; 
	size = __append_log(hctrl,db_buff,(uint32_t) db_start, (uint32_t) db_end,no_compressed);
	g_assert(size > 0);
    //HLOG_DEBUG("cur last offset:%d,log size:%d,next last offset:%d",hctrl->last_offset,size,hctrl->last_offset+size);
	hctrl->last_offset += size;
	return size;
}



/*
int append_inode(struct hlfs_ctrl * ctrl){
    HLOG_DEBUG("enter func %s", __func__);
    HLOG_DEBUG("leave func %s", __func__);
    return append_log(ctrl,NULL,0,0);
}*/

