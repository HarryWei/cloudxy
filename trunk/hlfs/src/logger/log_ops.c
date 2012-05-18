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
#include "storage.h"
#include "icache.h"




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
    uint32_t ib_offset      = db_offset + db_num * BLOCKSIZE ;
    guint32  db_cur_no = 0;
    for(db_cur_no = start_db; db_cur_no <= end_db; db_cur_no++){
        if (is_db_in_level1_index_range(db_cur_no)){
            //HLOG_DEBUG(" is level1 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            /*  write db to log buff  */
            int _idx = db_cur_no % 12;
            //HLOG_DEBUG(" idx:%u",_idx);
#if 1
            set_segno (&inode->blocks[_idx],last_segno);
            set_offset (&inode->blocks[_idx],last_offset + db_offset);
		    //HLOG_DEBUG("-----idx:%d, blocks:%lld,db_offset:%d",_idx,inode->blocks[_idx],db_offset);                
#endif
            db_offset += BLOCKSIZE;
        }else if (is_db_in_level2_index_range(db_cur_no)){
            if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == end_db){ 
#if 1
                set_segno (&inode->iblock, last_segno);
                set_offset (&inode->iblock, last_offset + ib_offset);
				//HLOG_DEBUG("-----iblock : %lld",inode->iblock);                
#endif
                ib_offset +=BLOCKSIZE;
            }
        }else if (is_db_in_level3_index_range(db_cur_no)){
            if((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == end_db){
                //HLOG_DEBUG(" save ib1");
#if 1
                set_segno (&inode->doubly_iblock,last_segno);
                set_offset(&inode->doubly_iblock,last_offset + ib_offset);
#endif
                ib_offset +=BLOCKSIZE;
            }
        }else if (is_db_in_level4_index_range(db_cur_no)){
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == end_db){
#if 1
                set_segno (&inode->triply_iblock,last_segno);
                set_offset(&inode->triply_iblock,last_offset + ib_offset);
#endif
                ib_offset +=BLOCKSIZE;
            }
        }
    }
    return 0;		 
}

static int update_icache(struct icache_ctrl *icctrl,char *log_iblock_buf,uint32_t db_start_no,uint32_t db_num){
    //HLOG_DEBUG("enter func %s", __func__);
    if(NULL == icctrl){
        //HLOG_DEBUG("icctrl is null");
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
            int ibno = get_layer1_ibno(i);
            //HLOG_DEBUG("ibno:%d",ibno);
            g_assert(ibno>=0);
            ret = icache_insert_iblock(icctrl,ibno,(char*)log_iblock_buf + offset);
            g_assert(ret==0);
            int  _idx = (i-12)%IB_ENTRY_NUM;
            uint64_t * ib=(uint64_t*)(log_iblock_buf + offset);
            uint64_t storage_address = *(ib+_idx);
            if((i - 12 + 1) % IB_ENTRY_NUM == 0 || i == db_start_no+db_num){
                offset += BLOCKSIZE;
            }   
        }else if(is_db_in_level3_index_range(i)){
            int ibno2 = get_layer2_ibno(i);
            //HLOG_DEBUG("ibno2:%d",ibno2);
            g_assert(ibno2>0);	
            ret = icache_insert_iblock(icctrl,ibno2,(char*)log_iblock_buf + offset);
            g_assert(ret==0);
            if((i -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || i == db_start_no+db_num){
                offset += BLOCKSIZE;
            }
            int ibno1 = get_layer1_ibno(i);
            //HLOG_DEBUG("ibno1:%d",ibno1);
            g_assert(ibno1>0);
            ret = icache_insert_iblock(icctrl,ibno1,(char*)log_iblock_buf + offset);
            g_assert(ret==0);	
            if((i - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 ||  i == db_start_no+db_num){
                offset += BLOCKSIZE;
            }
        }else if(is_db_in_level4_index_range(i)){
            int ibno3 = get_layer3_ibno(i);
            //HLOG_DEBUG("ibno3:%d",ibno3);
            g_assert(ibno3>0);		
            ret = icache_insert_iblock(icctrl,ibno3,(char*)log_iblock_buf + offset);
            g_assert(ret==0);
            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 ||i == db_start_no+db_num){
                offset += BLOCKSIZE;
            }
            int ibno2 = get_layer2_ibno(i);
            //HLOG_DEBUG("ibno2:%d",ibno2);
            g_assert(ibno2>0);		
            ret = icache_insert_iblock(icctrl,ibno2,(char*)log_iblock_buf + offset);
            g_assert(ret==0);
            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || i == db_start_no+db_num){
                offset += BLOCKSIZE;
            }
            int ibno1 = get_layer1_ibno(i);
            //HLOG_DEBUG("ibno3:%d",ibno1);
            g_assert(ibno1>0);
            ret = icache_insert_iblock(icctrl,ibno1,(char*)log_iblock_buf + offset);
            g_assert(ret==0);
            if((i-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 ||i == db_start_no+db_num){
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
            guint32 db_data_len = log->db_num * ctrl->sb.block_size;
            guint32 ib_offset   = db_data_len + LOG_HEADER_LENGTH;
            ret = update_icache(ctrl->icache,(char*)log + ib_offset,log->start_db_no,log->db_num);
            g_assert(ret==0);
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


int __append_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end){
    //HLOG_DEBUG("entry func: %s",__func__);
    gboolean ib1_flag  = FALSE;
    gboolean ib2_flag  = FALSE;
    gboolean ib3_flag  = FALSE;
	guint32 BLOCKSIZE = ctrl->sb.block_size;
	uint64_t*  ib1 = (uint64_t*)alloca(BLOCKSIZE);
	uint64_t*  ib2 = (uint64_t*)alloca(BLOCKSIZE);
	uint64_t*  ib3 = (uint64_t*)alloca(BLOCKSIZE);
	g_assert(NULL != ib1);
	g_assert(NULL != ib2);
	g_assert(NULL != ib3);
    guint32  ib_cur_offset = 0;
    guint32  db_offset = 0;
   
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
    guint32 db_data_len ;
    guint32 ib_data_len ;
    db_data_len = (db_end-db_start + 1) * BLOCKSIZE;
    ib_data_len = ib_amount(db_start, db_end) * BLOCKSIZE;
    char* log_buff = (char*)g_malloc0(db_data_len + ib_data_len + 
            sizeof(struct inode) + sizeof(struct inode_map_entry) + 
            sizeof(struct log_header));
    if (!log_buff) {
	    HLOG_ERROR("allocate error!");
        g_assert(0);
	    return -1;
    }
    guint32 ib_offset = db_data_len + LOG_HEADER_LENGTH;
    guint32 db_cur_no = 0;
    guint32 i=0;
    //HLOG_DEBUG(" db_data_len:%d ib_data_len:%d BLOCKSIZE:%d",db_data_len,ib_data_len,BLOCKSIZE);
    for(db_cur_no = db_start,i=0; db_cur_no <= db_end; db_cur_no++,i++){
        char * cur_block_ptr = (char *) (db_buff + i * BLOCKSIZE);  
        db_offset = LOG_HEADER_LENGTH + i*BLOCKSIZE;
        char * cur_log_buff_ptr = log_buff + db_offset;
        //HLOG_DEBUG(" db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
        if(is_db_in_level1_index_range(db_cur_no)){
            HLOG_DEBUG(" is level1 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            /*  write db to log buff  */
            int _idx = db_cur_no % 12;
            //HLOG_DEBUG(" idx:%u",_idx);
	     #if 0
            set_segno (&ctrl->inode.blocks[_idx],ctrl->last_segno);
            set_offset(&ctrl->inode.blocks[_idx],ctrl->last_offset + db_offset);
	     #endif
            //HLOG_DEBUG("inode.block[%d]'s storage addr:%llu",_idx,ctrl->inode.blocks[_idx]);
            //HLOG_DEBUG("log_buff:%p cur_log_buf:%p db_buff:%p cur_block_ptr:%p",
            //log_buff,cur_log_buff_ptr,db_buff,cur_block_ptr);
            //HLOG_DEBUG("log_buff:%p cur_log_buf:%p db_buff:%p cur_block_ptr:%p",
            //log_buff,cur_log_buff_ptr,db_buff,cur_block_ptr);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
        }else if(is_db_in_level2_index_range(db_cur_no)){
            //HLOG_DEBUG(" is level2 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //HLOG_DEBUG("-----iblock 0: %llu",ctrl->inode.iblock);                
            //uint64_t * _ib = NULL;
           if(FALSE == ib1_flag && ctrl->inode.iblock == 0){
                    memset(ib1,0,BLOCKSIZE);
					ib1_flag = TRUE;
           }else{
                    if(0>read_layer1_iblock(ctrl,db_cur_no,ib1)){ 	
                        if (0 != read_block_fast(ctrl,ctrl->inode.iblock,ib1)){
                            HLOG_ERROR("read block error!");
                            g_assert(0);
                            return -1;
                        }
			            write_layer1_iblock(ctrl,db_cur_no,ib1);	
                    }
            }
            int  _idx = (db_cur_no - 12)%IB_ENTRY_NUM;
            //uint64_t storage_address = *(_bi + _idx);
            set_segno ((ib1+_idx),ctrl->last_segno);
            set_offset ((ib1+_idx),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
            if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end ){
                //HLOG_DEBUG("set iblock - segno:%u",ctrl->last_segno);
		  #if 0
                set_segno(&ctrl->inode.iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.iblock,ctrl->last_offset + ib_offset);
		  #endif
                //HLOG_DEBUG("-----iblock get segno: %d",get_segno(ctrl->inode.iblock));
                memcpy((char*)(log_buff + ib_offset),(char*)ib1,BLOCKSIZE);
                //HLOG_DEBUG("-----iblock 2: %lld",ctrl->inode.iblock);                
                ib_offset +=BLOCKSIZE;
                //g_free(_ib);
                ib1_flag=FALSE;
            }
            //g_free(_ib);
        }else if(is_db_in_level3_index_range(db_cur_no)){
            //HLOG_DEBUG(" is level3 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
                if(FALSE == ib1_flag && ctrl->inode.doubly_iblock == 0){
                    memset(ib1,0,BLOCKSIZE);
					ib1_flag = TRUE;
                }else{
                    if(0>read_layer1_iblock(ctrl,db_cur_no,ib1)){ 	
                        if(0 != read_block_fast(ctrl,ctrl->inode.doubly_iblock,ib1)){
                            HLOG_ERROR("allocate error!");
                            g_assert(0);
                            return -1;
                        }
			   		write_layer1_iblock(ctrl,db_cur_no,ib1);
                    }
                }
       
            int _idx   = (db_cur_no - 12 - IB_ENTRY_NUM) / IB_ENTRY_NUM;
            //HLOG_DEBUG(" idx:%u",_idx);
            //uint64_t *_ib2 = NULL; 
                if(FALSE == ib2_flag && *(ib1+_idx) == 0 ){
                    memset(ib2,0,BLOCKSIZE);
					ib2_flag = TRUE;
                }else{
                    if(0>read_layer2_iblock(ctrl,db_cur_no,ib2)){ 	
                        if ( 0!=read_block_fast(ctrl,*(ib1+_idx),ib2)){
                            HLOG_ERROR("allocate error!");
                            g_assert(0);
                            return -1;
                        }
			   		write_layer2_iblock(ctrl,db_cur_no,ib2);
                    }
                }
          
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM)%IB_ENTRY_NUM;
            //HLOG_DEBUG(" idx2:%u",_idx2);
            set_segno ((ib2+_idx2),ctrl->last_segno);
            set_offset((ib2+_idx2),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);

            if((db_cur_no -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                //HLOG_DEBUG(" save ib2");
                set_segno ((ib1+_idx),ctrl->last_segno);
                set_offset((ib1+_idx),ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)ib2,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                //g_free(_ib2);
                ib2_flag=FALSE;
            }

            if((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
                //HLOG_DEBUG(" save ib1");
		  #if 0
                set_segno (&ctrl->inode.doubly_iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.doubly_iblock,ctrl->last_offset + ib_offset);
		  #endif
                memcpy(log_buff + ib_offset,(char*)ib1,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                //g_free(_ib);
                ib1_flag=FALSE;
            }
            //g_free(_ib);
            //g_free(_ib2);
        }else if(is_db_in_level4_index_range(db_cur_no)){
            //HLOG_DEBUG(" is level4 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
     
                if(FALSE == ib1_flag && ctrl->inode.triply_iblock == 0){
                     memset(ib1,0,BLOCKSIZE);
					 ib1_flag = TRUE;
                }else{
                    if(0>read_layer1_iblock(ctrl,db_cur_no,&ib1)){ 	
                        if ( 0 != read_block_fast(ctrl,ctrl->inode.triply_iblock,ib1)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
			        write_layer1_iblock(ctrl,db_cur_no,ib1);
                    }
                }
 
            int _idx   = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM * IB_ENTRY_NUM);
            //HLOG_DEBUG(" idx:%u",_idx);
            //uint64_t *_ib2 = NULL; 
 
                if(FALSE == ib2_flag && *(ib1+_idx) == 0 ){
                     memset(ib2,0,BLOCKSIZE);
					 ib2_flag = TRUE;
                }else{
                    if(0>read_layer2_iblock(ctrl,db_cur_no,ib2)){ 	
                        if (0 != read_block_fast(ctrl,*(ib1+_idx),ib2)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
			   		    write_layer2_iblock(ctrl,db_cur_no,ib2);
                    }
                }
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / IB_ENTRY_NUM % IB_ENTRY_NUM;
            //HLOG_DEBUG(" idx2:%u",_idx2);
            //uint64_t *_ib3 = NULL; 
   
                if(FALSE == ib3_flag && *(ib2+_idx2) == 0 ){
                     memset(ib3,0,BLOCKSIZE);
					 ib1_flag = TRUE;
                }else{
                    if(0>read_layer3_iblock(ctrl,db_cur_no,ib3)){ 	
                        if ( 0!= read_block_fast(ctrl,*(ib2+_idx2),ib3)){
                            HLOG_ERROR("allocate error!");
                            return -1;
                        }
                    }
		      		write_layer3_iblock(ctrl,db_cur_no,ib3);
                }
            int _idx3  = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
            //HLOG_DEBUG(" idx3:%u",_idx3);
            set_segno ((ib3+_idx3),ctrl->last_segno);
            set_offset((ib3+_idx3),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                set_segno ((ib2+_idx2),ctrl->last_segno);
                set_offset ((ib2+_idx2),ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)ib3,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                //g_free(_ib3);
                ib3_flag = FALSE;
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || db_cur_no == db_end){
                set_segno ((ib1+_idx),ctrl->last_segno);
                set_offset ((ib1+_idx),ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)ib2,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                //g_free(_ib2);
				ib2_flag = FALSE;
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
		  #if 0
                set_segno (&ctrl->inode.triply_iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.triply_iblock,ctrl->last_offset + ib_offset);
		  #endif
                memcpy(log_buff + ib_offset,(char*)ib1,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                //g_free(_ib);
				ib1_flag = FALSE;
            }
            //g_free(_ib);
            //g_free(_ib2);
            //g_free(_ib3);
        }else{
            /* over limit size  */
            //HLOG_ERROR("offset is out of limit size(8T)!!!");
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
               memcpy(log_buff +  offset,&ctrl->inode,sizeof(struct inode));
               memcpy(log_buff +  offset + sizeof(struct inode),&ctrl->imap_entry,sizeof(struct inode_map_entry));
               HLOG_DEBUG("to fill log header ...");
               struct log_header * lh = (struct log_header *)log_buff;
               lh->version = 0;
               //lh->header_checksum = 0;
               //lh->data_checksum = 0;
               lh->log_size = offset + sizeof(struct inode) + sizeof(struct inode_map_entry);
               //lh->ctime = get_current_time();
               lh->start_db_no = db_start;
               g_assert(db_data_len%BLOCKSIZE == 0);
               g_assert((ib_offset-db_data_len-LOG_HEADER_LENGTH)%BLOCKSIZE == 0);
               lh->db_num = db_data_len/BLOCKSIZE;
               lh->ib_num = (ib_offset - db_data_len - LOG_HEADER_LENGTH)/BLOCKSIZE;
               HLOG_DEBUG("log size:%d,log header:%d,inode:%d,inode map:%d,db:%d,ib:%d",lh->log_size,sizeof(struct log_header),sizeof(struct inode),sizeof(struct inode_map_entry),lh->db_num*BLOCKSIZE,lh->ib_num*BLOCKSIZE); 
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

int append_log(struct hlfs_ctrl *hctrl,const char *db_buff,uint32_t db_start,uint32_t db_end){
	guint32 BLOCKSIZE = hctrl->sb.block_size;
	int expand_size =	(db_end-db_start + 1)*BLOCKSIZE + ib_amount(db_start,db_end) * BLOCKSIZE + 
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
	//HLOG_DEBUG("last segno:%u last offset:%u", hctrl->last_segno,hctrl->last_offset);
	uint32_t size; 
	size = __append_log(hctrl,db_buff,(uint32_t) db_start, (uint32_t) db_end);
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

