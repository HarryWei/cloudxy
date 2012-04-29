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

int load_log(struct back_storage * storage, guint64 storage_address, struct log_header **log)
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    uint32_t offset = get_offset(storage_address); 
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(get_segno(storage_address),segfile_name);
    struct log_header lh;
    bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY);
    if (NULL == file) {
	    HLOG_ERROR("bs_file_open error!");
       ret = -1;
       goto out;
    } 
    if (LOG_HEADER_LENGTH != storage->bs_file_pread(storage,file, (char*)&lh, LOG_HEADER_LENGTH, offset)) {
	    HLOG_ERROR("bs_file_pread error!");
        ret = -1;
        goto out;
    }
    *log = (struct log_header *)g_malloc0(lh.log_size);
    if (!(*log)) {
	    HLOG_ERROR("allocate error");
	    ret = -1;
		goto out;
    }
    if (lh.log_size != storage->bs_file_pread(storage,file, (char*)*log, lh.log_size, offset)){
	    HLOG_ERROR("bs_file_pread error!");
        ret = -1;
        g_free(*log);
        goto out;
    }
out:
	if(0 != storage->bs_file_close(storage, file)) {
	    HLOG_ERROR("bs_file_close error!");
		ret = -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}

/*  return -1 mean spce  read fault
 *  return 1  mean space has not write yet */
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
        uint64_t *_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block error for iblock_addr:%u",ctrl->inode.iblock);
		return -1;
	}
        int  _idx = (db_no-12)%IB_ENTRY_NUM;
        storage_address = *(_ib+_idx);
        g_free(_ib);
    }else if (is_db_in_level3_index_range(db_no)){
        if(ctrl->inode.doubly_iblock ==0){
            return 1;
        }
        uint64_t *_ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.doubly_iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block error for doubly_iblock_addr:%u",ctrl->inode.doubly_iblock);
		return -1;
	}
        int _idx   = ( db_no - 12 - IB_ENTRY_NUM)/IB_ENTRY_NUM;
        if(*(_ib+_idx) == 0 ){
            return 1;
        }
        uint64_t *_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib+_idx),BLOCKSIZE);
        if(_ib2==NULL) {
		HLOG_ERROR("read_block error");
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
        uint64_t *_ib  = (uint64_t *)read_block(ctrl->storage,ctrl->inode.triply_iblock,BLOCKSIZE);
        if(_ib==NULL) {
		HLOG_ERROR("read_block error for triply_iblock_addr:%u",ctrl->inode.triply_iblock);
		return -1;
	}
        int _idx   = (db_no -12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM*IB_ENTRY_NUM);
        if(*(_ib + _idx) == 0){
            return 1;
        }
        uint64_t *_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib + _idx),BLOCKSIZE);
        if(_ib2==NULL) {
		HLOG_ERROR("read_block error");
		return -1;
	}
        int _idx2  = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM)/IB_ENTRY_NUM % IB_ENTRY_NUM;
        if(*(_ib2 + _idx2) == 0){
            return 1;
        }
        uint64_t *_ib3 = (uint64_t *)read_block(ctrl->storage,*(_ib2 + _idx2),BLOCKSIZE);
        if(_ib3==NULL) {
		HLOG_ERROR("read_block error");
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



#if 0
//TODO Just for a moment, will be removed if find bug
static build_segfile_name1(uint32_t segno, const char *segfile_name)
{
	if (NULL == segfile_name) {
		return -1;
	}
	//g_message("segfile_name is %p", segfile_name);
	snprintf((char *) segfile_name, SEGMENT_FILE_NAME_MAX, "%u%s%s", segno, ".", "seg");
	//g_message("segfile is %s", segfile_name);
	return 0;
}
#endif

//static bs_file_t g_cur_write_file = NULL;
static int dump_log(struct hlfs_ctrl *ctrl,struct log_header *log){
    HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    //gchar * path = (gchar*)ctrl->storage_handler;
    HLOG_DEBUG("ctrl last segno %d,offset:%d",ctrl->last_segno,ctrl->last_offset);
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(ctrl->last_segno,segfile_name);
    //char *segfile_path = g_build_filename(path,segfile_name,NULL);
    if(ctrl->last_offset==0){
        if(ctrl->cur_write_file_handler!=NULL){
	        if(0!=ctrl->storage->bs_file_close(ctrl->storage,(bs_file_t)ctrl->cur_write_file_handler)){
               HLOG_ERROR("close file failed");
               ret = -1;
               goto out;
            }
            ctrl->cur_write_file_handler ==NULL;
        }
        HLOG_DEBUG("need make a new segment:%d file",ctrl->last_segno);
        ctrl->cur_write_file_handler = (void*)ctrl->storage->bs_file_create(ctrl->storage,segfile_name);
    }else{
        if(ctrl->cur_write_file_handler==NULL){
            /*  open a exist file */
            HLOG_DEBUG("open a exist file............................");
            ctrl->cur_write_file_handler = (void*)ctrl->storage->bs_file_open(ctrl->storage,segfile_name,BS_WRITEABLE);
        }else{
	    	HLOG_DEBUG("...........");
        }
        HLOG_DEBUG("open a exist segment:%d file",ctrl->last_segno);
    }

    HLOG_DEBUG("cur write handler %p",ctrl->cur_write_file_handler);
    if(ctrl->cur_write_file_handler==NULL){
        HLOG_ERROR("fail to open segment file %s",segfile_name);
        ret = -1;
        goto out;
    }
    int size = ctrl->storage->bs_file_append(ctrl->storage,(bs_file_t)ctrl->cur_write_file_handler,(char*)log,log->log_size);
    HLOG_DEBUG("write to filewrite size %d  expect size %d",size,log->log_size);
    if(size != log->log_size){
       HLOG_DEBUG("write to file:%s failed, write size %d  expect size %d # %p",
		       segfile_name,size,log->log_size,ctrl->cur_write_file_handler);
       HLOG_ERROR("bs_file_append error");
       ret = -1;
       goto out;
    } 
out:
#if 0
    if(file != NULL) {
	    if(0!=ctrl->storage->bs_file_flush(ctrl->storage,file) ||
	        0!=ctrl->storage->bs_file_close(ctrl->storage,file)){
            g_message("storage flush failed");
            ret =-1;
        }
    }
#else
    if(ctrl->cur_write_file_handler!=NULL){
        if (0!=ctrl->storage->bs_file_flush(ctrl->storage,(bs_file_t)ctrl->cur_write_file_handler)){
            HLOG_ERROR("storage flush failed");
            ret =-1;
        }
#if 0
        if (0!=ctrl->storage->bs_file_close(ctrl->storage,(bs_file_t)ctrl->cur_write_file_handler)){
            g_message("storage close failed");
            ret =-1;
        }
        ctrl->cur_write_file_handler=NULL;
#endif 
    }
#endif 
    //g_free(segfile_name);
    HLOG_DEBUG("leave func %s", __func__);
    return ret;
}


int append_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end){
    HLOG_DEBUG("entry func: %s",__func__);
    uint64_t* _ib  = NULL;
    uint64_t* _ib2 = NULL;
    uint64_t* _ib3 = NULL;
    guint32  ib_cur_offset = 0;
    guint32  db_offset = 0;
    guint32 BLOCKSIZE = ctrl->sb.block_size;
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
#if 0
    if(db_buff == NULL){
        db_data_len = 0;
        ib_offset = db_data_len + LOG_HEADER_LENGTH;
        goto __inode_create;  
    }
#endif 
    guint32 db_cur_no = 0;
    guint32 i=0;
    HLOG_DEBUG(" db_data_len:%d ib_data_len:%d BLOCKSIZE:%d",db_data_len,ib_data_len,BLOCKSIZE);
    for(db_cur_no = db_start,i=0; db_cur_no <= db_end; db_cur_no++,i++){
        char * cur_block_ptr = (char *) (db_buff + i * BLOCKSIZE);  
        db_offset = LOG_HEADER_LENGTH + i*BLOCKSIZE;
        char * cur_log_buff_ptr = log_buff + db_offset;
        HLOG_DEBUG(" db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
        if(is_db_in_level1_index_range(db_cur_no)){
           HLOG_DEBUG(" is level1 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
           /*  write db to log buff  */
           int _idx = db_cur_no % 12;
           HLOG_DEBUG(" idx:%u",_idx);
           set_segno (&ctrl->inode.blocks[_idx],ctrl->last_segno);
           set_offset(&ctrl->inode.blocks[_idx],ctrl->last_offset + db_offset);
           HLOG_DEBUG("inode.block[%d]'s storage addr:%llu",_idx,ctrl->inode.blocks[_idx]);
           HLOG_DEBUG("log_buff:%p cur_log_buf:%p db_buff:%p cur_block_ptr:%p",
			   		log_buff,cur_log_buff_ptr,db_buff,cur_block_ptr);
           HLOG_DEBUG("log_buff:%p cur_log_buf:%p db_buff:%p cur_block_ptr:%p",
			   		log_buff,cur_log_buff_ptr,db_buff,cur_block_ptr);
           memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
        }else if(is_db_in_level2_index_range(db_cur_no)){
            HLOG_DEBUG(" is level2 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            HLOG_DEBUG("-----iblock 0: %llu",ctrl->inode.iblock);                
            //uint64_t * _ib = NULL;
            if(NULL == _ib){
                if(ctrl->inode.iblock == 0){
                    _ib = (uint64_t*)g_malloc0(BLOCKSIZE); 
                    if (NULL == _ib) {
                        HLOG_ERROR("g_malloc0 error!");
                        g_assert(0);
                        return -1;
                    }
                }else{
                    _ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.iblock,BLOCKSIZE);
                    if (NULL == _ib) {
                        HLOG_ERROR("read block error!");
                        g_assert(0);
                        return -1;
                    }
                }
            }
            int  _idx = (db_cur_no - 12)%IB_ENTRY_NUM;
            HLOG_DEBUG(" idx:%u",_idx);
            //uint64_t storage_address = *(_bi + _idx);
            set_segno ((_ib+_idx),ctrl->last_segno);
            set_offset((_ib+_idx),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);
            if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end ){
                HLOG_DEBUG("set iblock - segno:%u",ctrl->last_segno);
                set_segno(&ctrl->inode.iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.iblock,ctrl->last_offset + ib_offset);
                HLOG_DEBUG("-----iblock get segno: %d",get_segno(ctrl->inode.iblock));
                memcpy((char*)(log_buff + ib_offset),(char*)_ib,BLOCKSIZE);
                HLOG_DEBUG("-----iblock 2: %lld",ctrl->inode.iblock);                
                ib_offset +=BLOCKSIZE;
                g_free(_ib);
                HLOG_DEBUG("-----iblock 3: %lld",ctrl->inode.iblock);                
                _ib=NULL;
            }
            //g_free(_ib);
        }else if(is_db_in_level3_index_range(db_cur_no)){
            HLOG_DEBUG(" is level3 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
            if(NULL == _ib){
                if(ctrl->inode.doubly_iblock == 0){
                    _ib = (uint64_t *)g_malloc0(BLOCKSIZE);
    		    if (NULL==_ib) {
	    		    HLOG_ERROR("allocate error!");
                    g_assert(0);
	    		    return -1;
    			}
                }else{
                    _ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.doubly_iblock,BLOCKSIZE);
    			if (NULL==_ib) {
	    			HLOG_ERROR("allocate error!");
                    g_assert(0);
	    			return -1;
    			}
                }
            }
            int _idx   = (db_cur_no - 12 - IB_ENTRY_NUM) / IB_ENTRY_NUM;
            HLOG_DEBUG(" idx:%u",_idx);
            //uint64_t *_ib2 = NULL; 
            if(NULL == _ib2){
                if(*(_ib+_idx) == 0 ){
                    _ib2 = (uint64_t*)g_malloc0(BLOCKSIZE);
    			if (NULL==_ib2) {
	    			HLOG_ERROR("allocate error!");
                    g_assert(0);
	    			return -1;
    			}
                }else{
                    _ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib+_idx),BLOCKSIZE);
    			if (NULL==_ib2) {
	    			HLOG_ERROR("allocate error!");
                    g_assert(0);
	    			return -1;
    			}
                }
            }
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM)%IB_ENTRY_NUM;
            HLOG_DEBUG(" idx2:%u",_idx2);
            set_segno ((_ib2+_idx2),ctrl->last_segno);
            set_offset((_ib2+_idx2),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);

            if((db_cur_no -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
              HLOG_DEBUG(" save ib2");
              set_segno ((_ib+_idx),ctrl->last_segno);
              set_offset((_ib+_idx),ctrl->last_offset + ib_offset);
              memcpy(log_buff + ib_offset,(char*)_ib2,BLOCKSIZE);
              ib_offset +=BLOCKSIZE;
              g_free(_ib2);
              _ib2=NULL;
           }

           if((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
              HLOG_DEBUG(" save ib1");
              set_segno (&ctrl->inode.doubly_iblock,ctrl->last_segno);
              set_offset(&ctrl->inode.doubly_iblock,ctrl->last_offset + ib_offset);
              memcpy(log_buff + ib_offset,(char*)_ib,BLOCKSIZE);
              ib_offset +=BLOCKSIZE;
              g_free(_ib);
              _ib=NULL;
           }
           //g_free(_ib);
           //g_free(_ib2);
        }else if(is_db_in_level4_index_range(db_cur_no)){
            HLOG_DEBUG(" is level4 -- db_cur_no:%d db_offset:%d",db_cur_no,db_offset);
            //uint64_t *_ib = NULL;
            if(NULL == _ib){
                if(ctrl->inode.triply_iblock == 0){
                    _ib = (uint64_t *)g_malloc0(BLOCKSIZE);
    			if (NULL==_ib) {
	    			HLOG_ERROR("allocate error!");
	    			return -1;
    			}
                }else{
                    _ib = (uint64_t *)read_block(ctrl->storage,ctrl->inode.triply_iblock,BLOCKSIZE);
    				if (NULL==_ib) {
	    				HLOG_ERROR("allocate error!");
	    				return -1;
    				}
                }
            }
            int _idx   = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM * IB_ENTRY_NUM);
            HLOG_DEBUG(" idx:%u",_idx);
            //uint64_t *_ib2 = NULL; 
            if(NULL == _ib2){
                	if(*(_ib+_idx) == 0 ){
                    	_ib2 = (uint64_t*)g_malloc0(BLOCKSIZE);
    					if (NULL==_ib2) {
	    					HLOG_ERROR("allocate error!");
	    					return -1;
    					}
                	}else{
                    	_ib2 = (uint64_t *)read_block(ctrl->storage,*(_ib+_idx),BLOCKSIZE);
    					if (NULL==_ib2) {
	    					HLOG_ERROR("allocate error!");
	    					return -1;
    					}
                	}
            }
            int _idx2  = (db_cur_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / IB_ENTRY_NUM % IB_ENTRY_NUM;
            HLOG_DEBUG(" idx2:%u",_idx2);
            //uint64_t *_ib3 = NULL; 
            if(NULL == _ib3){
                if(*(_ib2+_idx2) == 0 ){
                    _ib3 = (uint64_t*)g_malloc0(BLOCKSIZE);
    			if (NULL==_ib3) {
	    			HLOG_ERROR("allocate error!");
	    			return -1;
    			}
                }else{
                    _ib3 = (uint64_t *)read_block(ctrl->storage,*(_ib2+_idx2),BLOCKSIZE);
    			if (NULL==_ib3) {
	    			HLOG_ERROR("allocate error!");
	    			return -1;
    			}
                }
            }
            int _idx3  = (db_cur_no -12 -IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) % IB_ENTRY_NUM; 
            HLOG_DEBUG(" idx3:%u",_idx3);
            set_segno ((_ib3+_idx3),ctrl->last_segno);
            set_offset((_ib3+_idx3),ctrl->last_offset + db_offset);
            memcpy(cur_log_buff_ptr,cur_block_ptr,BLOCKSIZE);

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                set_segno ((_ib2+_idx2),ctrl->last_segno);
                set_offset((_ib2+_idx2),ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)_ib3,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                g_free(_ib3);
                _ib3=NULL;
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || db_cur_no == db_end){
                set_segno ((_ib+_idx),ctrl->last_segno);
                set_offset((_ib+_idx),ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)_ib2,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                g_free(_ib2);
                _ib2=NULL;
            }

            if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
                set_segno (&ctrl->inode.triply_iblock,ctrl->last_segno);
                set_offset(&ctrl->inode.triply_iblock,ctrl->last_offset + ib_offset);
                memcpy(log_buff + ib_offset,(char*)_ib,BLOCKSIZE);
                ib_offset +=BLOCKSIZE;
                g_free(_ib);
                _ib=NULL;
            }
            //g_free(_ib);
            //g_free(_ib2);
            //g_free(_ib3);
        }else{
           /* over limit size  */
           HLOG_ERROR("offset is out of limit size(8T)!!!");
           return -1;   
        }
   }
   __inode_create:;
        HLOG_DEBUG("to update inode ...");
        int offset = ib_offset;
        HLOG_DEBUG("to update inode map entry ...");
        HLOG_DEBUG("last offset:%u , last segno:%u log head len:%d iboffset:%d", ctrl->last_offset,ctrl->last_segno,LOG_HEADER_LENGTH,ib_offset);
        ctrl->imap_entry.inode_no = HLFS_INODE_NO; 
        set_segno (&ctrl->imap_entry.inode_addr,ctrl->last_segno);     
        set_offset(&ctrl->imap_entry.inode_addr,ctrl->last_offset + offset);    
        HLOG_DEBUG("inode address's offset %llu , give it %u",ctrl->imap_entry.inode_addr,ctrl->last_offset + offset);
        memcpy(log_buff +  offset,&ctrl->inode,sizeof(struct inode));
        memcpy(log_buff +  offset + sizeof(struct inode),&ctrl->imap_entry,sizeof(struct inode_map_entry));
        HLOG_DEBUG("to fill log header ...");
        struct log_header * lh = (struct log_header *)log_buff;
        lh->version = 0;
        lh->header_checksum = 0;
        lh->data_checksum = 0;
        lh->log_size = offset + sizeof(struct inode) + sizeof(struct inode_map_entry);
        lh->ctime = get_current_time();
        lh->start_db_no = db_start;
        g_assert(db_data_len%BLOCKSIZE == 0);
        g_assert((ib_offset-db_data_len-LOG_HEADER_LENGTH)%BLOCKSIZE == 0);
        lh->db_num = db_data_len/BLOCKSIZE;
        lh->ib_num = (ib_offset - db_data_len - LOG_HEADER_LENGTH)/BLOCKSIZE;
        HLOG_DEBUG("log size:%d,log header:%d,inode:%d,inode map:%d,db:%d,ib:%d",lh->log_size,sizeof(struct log_header),sizeof(struct inode),sizeof(struct inode_map_entry),lh->db_num*BLOCKSIZE,lh->ib_num*BLOCKSIZE); 
        if(0!=dump_log(ctrl,lh)){
           HLOG_ERROR("log dump failed");
           g_assert(0);
        }
        int size = lh->log_size;
        HLOG_DEBUG("return log size :%d",lh->log_size);
        g_free(log_buff);
    	HLOG_DEBUG("leave func %s", __func__);
        return size;
}

int append_inode(struct hlfs_ctrl * ctrl){
    HLOG_DEBUG("enter func %s", __func__);
    HLOG_DEBUG("leave func %s", __func__);
    return append_log(ctrl,NULL,0,0);
}
