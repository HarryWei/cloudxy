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


int read_layer1_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char **iblock){
     HLOG_DEBUG("enter func %s", __func__);
     int ret;
     if(NULL != hctrl->icache){
	     int ibno = get_layer1_ibno(dbno);
	     g_assert(ibno >= 0);
	     ret =  icache_query_iblock(hctrl->icache,ibno,*iblock);
	     if(ret == 0){
		   return -1; 
	     }
     }	
     return -1;	 
}	
int read_layer2_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char **iblock){
     HLOG_DEBUG("enter func %s", __func__);
     int ret;
     if(NULL != hctrl->icache){
	     int ibno = get_layer2_ibno(dbno);
	     g_assert(ibno >= 0);
	     ret =  icache_query_iblock(hctrl->icache,ibno,*iblock);
	     if(ret == 0){
		   return -1; 
	     }
     }	
     return -1;	
}	
int read_layer3_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char **iblock){
     HLOG_DEBUG("enter func %s", __func__);
     int ret;
     if(NULL != hctrl->icache){
	     int ibno = get_layer3_ibno(dbno);
	     g_assert(ibno >= 0);
	     ret =  icache_query_iblock(hctrl->icache,ibno,*iblock);
	     if(ret == 0){
		   return -1; 
	     }
     }	
     return -1;	
}	

/* 
 *   return -1 mean spce  read fault
 *   return 1  mean space has not write yet 
 */
static int pre_open_read_segfile(struct hlfs_ctrl *ctrl,uint32_t segno){
	HLOG_DEBUG("enter func %s", __func__);
    if(ctrl->cur_read_file_handler==NULL){
       HLOG_DEBUG("open cur read file handler ,no:%d",segno);
       const char segfile_name[SEGMENT_FILE_NAME_MAX];
       build_segfile_name(segno,segfile_name);
       bs_file_t file = ctrl->storage->bs_file_open(ctrl->storage,segfile_name,BS_READONLY); 
       if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segfile_name);
            g_assert(0);
            return -1;
       }
       ctrl->cur_read_file_handler = file;
       ctrl->cur_read_segno =segno;
    }else if(ctrl->cur_read_segno != segno){
       HLOG_DEBUG("cur segno no:%d,give no:%d",ctrl->cur_read_segno,segno);
       /* open file ...  */
       bs_file_t file = ctrl->cur_read_file_handler;
       if(ctrl->cur_read_file_handler!=ctrl->cur_write_file_handler){
          HLOG_DEBUG("close pre open read file handler");
          ctrl->storage->bs_file_close(ctrl->storage,file);
       }else{
          HLOG_DEBUG("do not close pre open read file handler");
       }
       const char segfile_name[SEGMENT_FILE_NAME_MAX];
       build_segfile_name(segno,segfile_name);
       //gchar * segfile_name = build_segfile_name(segno);
       file = ctrl->storage->bs_file_open(ctrl->storage,segfile_name,BS_READONLY); 
       if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segfile_name);
            return -1;
       }
       ctrl->cur_read_file_handler = file;
       ctrl->cur_read_segno =segno;
    }else{
       HLOG_DEBUG("using pre open read file handler");
    }
	HLOG_DEBUG("leave func %s", __func__);
   return 0;
}

static int pre_close_read_segfile(struct hlfs_ctrl *ctrl,uint32_t segno){
	HLOG_DEBUG("enter func %s", __func__);
    if(ctrl->last_segno == segno){
       HLOG_DEBUG("pre close read segfile %d",segno);
       ctrl->storage->bs_file_close(ctrl->storage,ctrl->cur_read_file_handler);
       ctrl->cur_read_file_handler=NULL;
    } 
	HLOG_DEBUG("leave func %s", __func__);
    return 0;
}
static char *read_block_fast(struct hlfs_ctrl *ctrl,uint64_t storage_address,uint32_t block_size)
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    int write_size = 0;
    uint32_t offset = get_offset(storage_address);
    uint32_t segno = get_segno(storage_address);
	HLOG_DEBUG("offset :%u,segno:%u",offset,segno);
    if(0!=pre_open_read_segfile(ctrl,segno)){
	   HLOG_ERROR("can not pre open read segfile:%u",segno);
       return NULL; 
    }
    char * block = (char*)g_malloc0(block_size);
    if (NULL == block) {
	    HLOG_ERROR("Allocate Error!");
	    block = NULL;
	    goto out;
    }
    do{
        //if(block_size != (write_size = ctrl->storage->bs_file_pread(ctrl->storage,ctrl->cur_read_file_handler,block,block_size,offset))){
        write_size = ctrl->storage->bs_file_pread(ctrl->storage,ctrl->cur_read_file_handler,block,block_size,offset);
        if(write_size!=block_size){
            HLOG_ERROR("can not read block from seg:%u#%u :ret :%d",segno,offset,write_size);
            sleep(1);
        }
            //g_free(block);
            //block = NULL;
            //goto out;
    }while(write_size < block_size);
out:
    pre_close_read_segfile(ctrl,segno);
	HLOG_DEBUG("leave func %s", __func__);
    return block;
}

int load_block_by_no(struct hlfs_ctrl *ctrl,uint64_t no,char **block){
	HLOG_DEBUG("enter func %s", __func__);
    int ret =0;
    if(ctrl->cctrl!=NULL){
	   HLOG_DEBUG("read from cache first");
       *block = g_malloc0(ctrl->cctrl->block_size);
       ret = cache_query_block(ctrl->cctrl,no,*block);
       if(ret == 0 ){
	      HLOG_DEBUG("read from cache!");
          return 0;
       }
       g_free(*block);
	   HLOG_DEBUG("not find in cache!");
    }
    uint64_t storage_address ;
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint32_t db_no = no;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
    if(is_db_in_level1_index_range(db_no)){
        int _idx = db_no % 12;
        storage_address = ctrl->inode.blocks[_idx];
    }else if (is_db_in_level2_index_range(db_no)){
        if(ctrl->inode.iblock == 0){
          return 1;
        }	
	 uint64_t *_ib=NULL;
	 if(0!=read_layer1_iblock(ctrl,db_no,&_ib)){
        	_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.iblock,BLOCKSIZE);
        	if(_ib==NULL) {
			HLOG_ERROR("read_block error for iblock_addr:%llu",ctrl->inode.iblock);
			return -1;
	 	}
	 }	
        int  _idx = (db_no-12)%IB_ENTRY_NUM;
        storage_address = *(_ib+_idx);
        g_free(_ib);
    }else if (is_db_in_level3_index_range(db_no)){
        if(ctrl->inode.doubly_iblock ==0){
            return 1;
        }
        uint64_t *_ib=NULL;
	 if(0!=read_layer1_iblock(ctrl,db_no,&_ib)){
	 	_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.doubly_iblock,BLOCKSIZE);
        	if(_ib==NULL) {
		HLOG_ERROR("read_block error for doubly_iblock_addr:%llu",ctrl->inode.doubly_iblock);
		return -1;
	 	}
	 }	
        int _idx   = ( db_no - 12 - IB_ENTRY_NUM)/IB_ENTRY_NUM;
        if(*(_ib+_idx) == 0 ){
            return 1;
        }
        uint64_t *_ib2=NULL;
	 if(0!=read_layer2_iblock(ctrl,db_no,&_ib2)){
		 _ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib+_idx),BLOCKSIZE);
        	if(_ib2==NULL) {
			HLOG_ERROR("read_block error");
			return -1;
		}
	}
        int _idx2  = (db_no - 12 - IB_ENTRY_NUM)%IB_ENTRY_NUM;
        storage_address = *(_ib2 + _idx2);
        g_free(_ib);
        g_free(_ib2);
    }else if (is_db_in_level4_index_range(db_no)){
        if(ctrl->inode.triply_iblock == 0){
            return 1;
        }
        uint64_t *_ib = NULL;
	 if(0!=read_layer1_iblock(ctrl,db_no,&_ib)){	
	 	_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.triply_iblock,BLOCKSIZE);
        	if(_ib==NULL) {
			HLOG_ERROR("read_block error for triply_iblock_addr:%llu",ctrl->inode.triply_iblock);
			return -1;
		}
	 }
        int _idx   = (db_no -12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM*IB_ENTRY_NUM);
        if(*(_ib + _idx) == 0){
            return 1;
        }
        uint64_t *_ib2 = NULL;
	 if(0!=read_layer2_iblock(ctrl,db_no,&_ib2)){
	 	_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib + _idx),BLOCKSIZE);
        	if(_ib2==NULL) {
			HLOG_ERROR("read_block error");
			return -1;
		}
	}	
        int _idx2  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM)/IB_ENTRY_NUM % IB_ENTRY_NUM;
        if(*(_ib2 + _idx2) == 0){
            return 1;
        }
        uint64_t *_ib3 = NULL;
	 if(0!=read_layer3_iblock(ctrl,db_no,&_ib3)){
	 	_ib3 = (uint64_t *)read_block(ctrl->storage,*(_ib2 + _idx2),BLOCKSIZE);
        	if(_ib3==NULL) {
			HLOG_ERROR("read_block error");
			return -1;
		}
	 }
        int _idx3  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
        storage_address = *(_ib3 + _idx3);
        g_free(_ib);
        g_free(_ib2);
        g_free(_ib3);
    }
    HLOG_DEBUG("storage_address: %llu", storage_address);
    if(storage_address == 0){
        return 1;
    }
    *block = read_block(ctrl->storage,storage_address,BLOCKSIZE);
    if(*block ==NULL){
	  HLOG_ERROR("can not read block for storage address %llu", storage_address);
         return -1;
    }
    HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

int load_block_by_no_fast(struct hlfs_ctrl *ctrl,uint64_t no,char **block){
    HLOG_DEBUG("enter func %s", __func__);
    int ret=0;
    if(ctrl->cctrl!=NULL){
        HLOG_DEBUG("read from cache first");
        *block = g_malloc0(ctrl->cctrl->block_size);
        ret = cache_query_block(ctrl->cctrl,no,*block);
        if(ret == 0 ){
            HLOG_DEBUG("read from cache!");
            return 0;
        }
        g_free(*block);
        HLOG_DEBUG("not find in cache!");
    }
    uint64_t storage_address ;
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint32_t db_no = no;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
    if(is_db_in_level1_index_range(db_no)){
        int _idx = db_no % 12;
        storage_address = ctrl->inode.blocks[_idx];
    }else if (is_db_in_level2_index_range(db_no)){
        if(ctrl->inode.iblock == 0){
            return 1;
        }
        uint64_t *_ib = (uint64_t *)read_block_fast(ctrl,ctrl->inode.iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	 }
        int  _idx = (db_no-12)%IB_ENTRY_NUM;
        storage_address = *(_ib+_idx);
        g_free(_ib);
    }else if (is_db_in_level3_index_range(db_no)){
        if(ctrl->inode.doubly_iblock ==0){
            return 1;
        }
        uint64_t *_ib = (uint64_t *)read_block_fast(ctrl,ctrl->inode.doubly_iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	}
        int _idx   = ( db_no - 12 - IB_ENTRY_NUM)/IB_ENTRY_NUM;
        if(*(_ib+_idx) == 0 ){
            return 1;
        }
        uint64_t *_ib2 = (uint64_t *)read_block_fast(ctrl,*(_ib+_idx),BLOCKSIZE);
        if(_ib2==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	}
        int _idx2  = (db_no - 12 - IB_ENTRY_NUM)%IB_ENTRY_NUM;
        storage_address = *(_ib2 + _idx2);
        g_free(_ib);
        g_free(_ib2);
    }else if (is_db_in_level4_index_range(db_no)){
        if(ctrl->inode.triply_iblock == 0){
            return 1;
        }
        uint64_t *_ib  = (uint64_t *)read_block_fast(ctrl,ctrl->inode.triply_iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	}
        int _idx   = (db_no -12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM*IB_ENTRY_NUM);
        if(*(_ib + _idx) == 0){
            return 1;
        }
        uint64_t *_ib2 = (uint64_t *)read_block_fast(ctrl,*(_ib + _idx),BLOCKSIZE);
        if(_ib2==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	}
        int _idx2  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM)/IB_ENTRY_NUM % IB_ENTRY_NUM;
        if(*(_ib2 + _idx2) == 0){
            return 1;
        }
        uint64_t *_ib3 = (uint64_t *)read_block_fast(ctrl,*(_ib2 + _idx2),BLOCKSIZE);
        if(_ib3==NULL) {
		HLOG_ERROR("read_block_fast error");
		return -1;
	}
        int _idx3  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
        storage_address = *(_ib3 + _idx3);
        g_free(_ib);
        g_free(_ib2);
        g_free(_ib3);
    }
    HLOG_DEBUG("storage_address: %llu", storage_address);
    if(storage_address == 0){
        return 1;
    }
    *block = read_block_fast(ctrl,storage_address,BLOCKSIZE);
    if(*block ==NULL){
	HLOG_ERROR("can not read block for storage address %llu", storage_address);
       return -1;
    }
    HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

int load_block_by_addr_fast(struct hlfs_ctrl *ctrl,uint64_t pos,char** block){
    HLOG_DEBUG("enter func %s", __func__);
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint32_t db_no = pos /BLOCKSIZE;
    HLOG_DEBUG("leave func %s", __func__);
    return load_block_by_no_fast(ctrl,db_no,block);
}

int load_block_by_addr(struct hlfs_ctrl *ctrl,uint64_t pos,char** block){
    HLOG_DEBUG("enter func %s", __func__);
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint32_t db_no = pos /BLOCKSIZE;
    HLOG_DEBUG("leave func %s", __func__);
    return load_block_by_no(ctrl,db_no,block);
}




