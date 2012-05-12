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



int __read_layer_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,int layerno,uint64_t **iblock_buf){
    HLOG_DEBUG("enter func %s", __func__);
    int ret;
    int ibno ;
    if(NULL != hctrl->icache){
        if(layerno == 1){
            ibno = get_layer1_ibno(dbno);
        }else if(layerno == 2){
            ibno = get_layer2_ibno(dbno);
        }else if(layerno == 3){
            ibno = get_layer3_ibno(dbno);
        }else{
            g_assert(0);
        }
        HLOG_DEBUG("ibno:%d",ibno);
        g_assert(ibno >= 0);
        if(TRUE == icache_iblock_exist(hctrl->icache,ibno)){
            *iblock_buf = g_malloc0(hctrl->icache->iblock_size);	
            ret =  icache_query_iblock(hctrl->icache,ibno,(char*)*iblock_buf);
            if(0 != ret){
                HLOG_ERROR(" can not find iblock in icache ");
                return -1; 
            }
        }else{
            return -1;   
        }
        return 0;	
    }
}

int read_layer1_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,uint64_t **iblock){
    return __read_layer_iblock(hctrl,dbno,1,iblock);	
}	
int read_layer2_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,uint64_t **iblock){
    HLOG_DEBUG("enter func %s", __func__);
    return __read_layer_iblock(hctrl,dbno,2,iblock);	
}	
int read_layer3_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,uint64_t **iblock){
    HLOG_DEBUG("enter func %s", __func__);
    return __read_layer_iblock(hctrl,dbno,3,iblock);	
}	


__write_layer_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,int layerno,char *iblock){
	HLOG_DEBUG("enter func %s", __func__);
    int ret;
    if(NULL != hctrl->icache){
	     int ibno ;
		 if(layerno == 1){
		    ibno = get_layer1_ibno(dbno);
		 }else if(layerno == 2){
		    ibno = get_layer2_ibno(dbno);
         }else if(layerno == 3){
            ibno = get_layer3_ibno(dbno);
         }else{
            g_assert(0);
         }
	     g_assert(ibno >= 0);
	     ret = icache_insert_iblock(hctrl->icache,ibno,iblock);
    }	
    return ret;	
}

int write_layer1_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char *iblock){
     return __write_layer_iblock(hctrl,dbno,1,iblock);	
}	
int write_layer2_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char *iblock){
     HLOG_DEBUG("enter func %s", __func__);
     return __write_layer_iblock(hctrl,dbno,2,iblock);	
}	
int write_layer3_iblock(struct hlfs_ctrl *hctrl,uint64_t dbno,char *iblock){
     HLOG_DEBUG("enter func %s", __func__);
     return __write_layer_iblock(hctrl,dbno,3,iblock);	
}	

/******************************************************************************************/

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
        //if(block_size != (write_size = ctrl->storage->bs_file_pread(ctrl->storage,ctrl->last_rsegfile_handler,block,block_size,offset))){
        write_size = ctrl->storage->bs_file_pread(ctrl->storage,ctrl->last_rsegfile_handler,block,block_size,offset);
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

/**************************************************/


int load_block_by_no(struct hlfs_ctrl *ctrl,uint64_t no,char **block){
	HLOG_DEBUG("enter func %s,no:%d", __func__,no);
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
	 if(0>read_layer1_iblock(ctrl,db_no,&_ib)){
        	if(NULL == (_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.iblock,BLOCKSIZE))){
			   HLOG_ERROR("read_block error for iblock_addr:%llu",ctrl->inode.iblock);
			   return -1;
	 	    }
		    write_layer1_iblock(ctrl,db_no,_ib);	
	 }

        int  _idx = (db_no-12)%IB_ENTRY_NUM;
        storage_address = *(_ib+_idx);
        g_free(_ib);
    }else if (is_db_in_level3_index_range(db_no)){
        if(ctrl->inode.doubly_iblock ==0){
            return 1;
        }
        uint64_t *_ib=NULL;
	 if(0>read_layer1_iblock(ctrl,db_no,&_ib)){
	 	if(NULL == (_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.doubly_iblock,BLOCKSIZE))){
        	
				HLOG_ERROR("read_block error for doubly_iblock_addr:%llu",ctrl->inode.doubly_iblock);
				return -1;
		 	}
			write_layer1_iblock(ctrl,db_no,_ib);	
	 }	
        int _idx   = ( db_no - 12 - IB_ENTRY_NUM)/IB_ENTRY_NUM;
        if(*(_ib+_idx) == 0 ){
            return 1;
        }
        uint64_t *_ib2=NULL;
	 if(0>read_layer2_iblock(ctrl,db_no,&_ib2)){
		if(NULL == (_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib+_idx),BLOCKSIZE))){	
			HLOG_ERROR("read_block error");
			return -1;
		}
		write_layer2_iblock(ctrl,db_no,_ib2);
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
	 if(0>read_layer1_iblock(ctrl,db_no,&_ib)){	
	 	 if(NULL == (_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.triply_iblock,BLOCKSIZE))){
        
			HLOG_ERROR("read_block error for triply_iblock_addr:%llu",ctrl->inode.triply_iblock);
			return -1;
		}
			write_layer1_iblock(ctrl,db_no,_ib);
	 }
        int _idx   = (db_no -12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM*IB_ENTRY_NUM);
        if(*(_ib + _idx) == 0){
            return 1;
        }
        uint64_t *_ib2 = NULL;
	 if(0>read_layer2_iblock(ctrl,db_no,&_ib2)){
	 	 if(NULL == (_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib + _idx),BLOCKSIZE))){
			HLOG_ERROR("read_block error");
			return -1;
		}
		write_layer2_iblock(ctrl,db_no,_ib2);	
	}	
        int _idx2  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM)/IB_ENTRY_NUM % IB_ENTRY_NUM;
        if(*(_ib2 + _idx2) == 0){
            return 1;
        }
        uint64_t *_ib3 = NULL;
	 if(0>read_layer3_iblock(ctrl,db_no,&_ib3)){
	 	if(NULL == (_ib3 = (uint64_t *)read_block(ctrl->storage,*(_ib2 + _idx2),BLOCKSIZE))){
			HLOG_ERROR("read_block error");
			return -1;
		}
		write_layer3_iblock(ctrl,db_no,_ib3);	
	 }
        int _idx3  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
        storage_address = *(_ib3 + _idx3);
        g_free(_ib);
        g_free(_ib2);
        g_free(_ib3);
    }
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
