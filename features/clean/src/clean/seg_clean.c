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




#if 0
int load_seg_usage(,uint32_t segno,SEG_USAGE_T * seg_usage){

}

int load_seg_usage(struct back_storage * storage,
		SEG_USAGE_T* seg_usage,uint32_t segno){
     HLOG_DEBUG("enter func %s",__func__);
     const char segfile[SEGMENT_FILE_NAME_MAX];
     build_segfile_name(segno,segfile);
     gchar *seg_usage_file = g_strconcat(segfile,".usage",NULL); 
     bs_file_t file = storage->bs_file_open(storage,seg_usage_file,BS_READONLY);      
     if(NULL==file){
        g_free(seg_usage_file);
	HLOG_ERROR("file is null");
        return -1;
     }
     char textbuf[8192];
     memset(textbuf,0,8192);
     uint32_t count = storage->bs_file_pread(storage,file,textbuf,8192,0);
     if(count < 0){
        g_free(seg_usage_file);
        storage->bs_file_close(storage,file);
	HLOG_ERROR("count is lower than 0");
        return -1;
     }
     
     load_segment_usage_from_text(storage,seg_usage,textbuf);
     g_free(seg_usage_file);
     storage->bs_file_close(storage,file);
       HLOG_DEBUG("leave func %s",__func__);
     return 0;  
}



struct segment_usage *get_segment_usage(uint32_t segno){
    HLOG_DEBUG( "enter func %s",__func__);
    HLOG_DEBUG("leave func %s",__func__);
    return NULL;    
}

#endif 

int seg_usage_calc(struct back_storage* storage, uint32_t segment_size,uint32_t block_size,uint64_t segno,struct inode *refer_inode,SEG_USAGE_T *seg_usage)
{
    HLOG_DEBUG("enter func %s",__func__);
    if(storage == NULL || refer_inode == NULL || SEG_USAGE_T == NULL){
		return -1;
    }
		
    int ret = 0;
    int log_idx=0;
    int idx;
    uint32_t offset = 0; 
    struct log_header *lh;
    gchar **v = g_strsplit(segfile,".",2);
    uint64_t db_mine_storage_addr_segno = atol(v[0]);
    g_strfreev(v);

    GArray *tmp_bit_array;
    seg_usage->segno = db_mine_storage_addr_segno; 
    seg_usage->timestamp = latest_inode->ctime;
    HLOG_DEBUG("seg usage's segno:%llu,timestamp:%llu",seg_usage->segno,seg_usage->timestamp);
    bs_file_t file = storage->bs_file_open(storage,segfile,BS_READONLY);
    if (NULL == file) {
        HLOG_ERROR("open segfile:%s failed",segfile);
        return -1;
    } 
    tmp_bit_array = g_array_new(FALSE,FALSE,sizeof(gint));

   
    char *tmp_buf = (char*)g_malloc0(SEGMENT_SIZE); /*  suppose segment size < 64M */
    int count =storage->bs_file_pread(storage,file,tmp_buf,SEGMENT_SIZE,0);
    if(count<0){
        HLOG_ERROR("read content failed");
    }

    while(offset < count){
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
        lh = (struct log_header*)(tmp_buf + offset);
        if(seg_usage->log_num !=0){
            HLOG_DEBUG("this segfile:%s has calc",segfile);
            idx = log_idx/8;
            if(!(seg_usage->bitmap[idx] & (1<<(log_idx%8)))){
                log_idx++;
                int x=0;
                g_array_append_val(tmp_bit_array,x);
                offset += lh->log_size;
                continue;
            }
        }
        uint64_t orgine_alive_blocks = seg_usage->alive_blocks;
        HLOG_DEBUG("start db no:%llu,db num:%d",lh->start_db_no,lh->db_num);
        int i;
#if 1
        for(i=0;i<lh->db_num;i++){
            HLOG_DEBUG("for db:%llu",lh->start_db_no+i);
            uint64_t db_mine_storage_addr = 0;
            uint64_t db_mine_storage_addr_offset = offset+LOG_HEADER_LENGTH+i*block_size;
            set_offset(&db_mine_storage_addr,db_mine_storage_addr_offset);
            set_segno(&db_mine_storage_addr,db_mine_storage_addr_segno);
            uint64_t db_cur_storage_addr = get_db_storage_addr_in_inode(storage,latest_inode,
			    						lh->start_db_no+i,block_size);
            HLOG_DEBUG("db:%llu's mine storage addr:%llu,cur storage addr:%llu",
			    	lh->start_db_no+i,db_mine_storage_addr,db_cur_storage_addr);
            if(db_mine_storage_addr != db_cur_storage_addr){
                HLOG_DEBUG("this is overwrite data block");

            }else{
                seg_usage->alive_blocks++;
                HLOG_DEBUG("this is used data block :%llu",seg_usage->alive_blocks);
            }
        }
#endif
        //uint32_t alive_blocks = log_usage_calc(storage,latest_inode,&lh,db_mine_storage_addr_segno,offset,block_size);
        if(orgine_alive_blocks == seg_usage->alive_blocks){
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
    seg_usage->bitmap = (char*)g_malloc0((seg_usage->log_num-1)/8+1);
    HLOG_DEBUG("size of bitmap:%d",tmp_bit_array->len);
    for(i=0;i<tmp_bit_array->len;i++){
        gint value = g_array_index(tmp_bit_array,gint,i);
        idx = i/8;
        if(value==1){
            //g_message("bitmap idx bit:%d = 1\n",i);
           seg_usage->bitmap[idx] |= 1<<i%8;
           //g_message("bitmap idx %x\n",seg_usage->bitmap[idx]);
       }
    }

    g_array_free(tmp_bit_array,TRUE);
	HLOG_DEBUG("leave func %s",__func__);
    return 0;
}

int dump_seg_usage(struct back_storage * storage,
    const char*segment_usage_file,
    struct segment_usage * seg_usage){
    HLOG_DEBUG("enter func %s",__func__);
    HLOG_DEBUG("enter func %s,seg usage file:%s",__func__,segment_usage_file);
    char segtextbuf[sizeof(struct segment_usage)*10];
    memset(segtextbuf,0,sizeof(struct segment_usage)*10);
    uint32_t len = segment_usage2text(seg_usage,segtextbuf);
    int ret = dump_segment_usage_text(storage,segment_usage_file,segtextbuf);
    HLOG_DEBUG("leave func %s",__func__);
    return ret;
}



int rewrite_alive_blocks(struct hlfs_ctrl *ctrl,struct segment_usage *seg_usage){
    HLOG_DEBUG("enter func %s",__func__);
    int ret = 0;
    const char segfile[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(seg_usage->segno,segfile);
	bs_file_t file = NULL; 
    if (NULL == (file = ctrl->storage->bs_file_open(ctrl->storage, segfile, BS_READONLY))) {
			HLOG_ERROR("open segfile error!");
			return -1;
	}
    char * buff = (char*)g_malloc0(1024llu*1024llu*64llu);
	uint32_t count = ctrl->storage->bs_file_pread(ctrl->storage,file, 
							buff, 1024llu*1024llu*64llu, 0);
    if(count<0){
	    HLOG_ERROR("bs_file_pread error!");
	    ctrl->storage->bs_file_close(ctrl->storage, file);
		return -1;
	}
    int i= 0;
    uint32_t offset=0;
#if 0
    struct log_header *lh = (struct log_header *)g_malloc0(sizeof(struct log_header));
    if (!lh) {
        g_message("%s -- Allocate Error!\n");
        return -1;
    }
#endif
#if 1
    struct log_header *lh = NULL;
#endif
    HLOG_DEBUG("Before Allocate!");
    HLOG_DEBUG("log_num: --> %llu", seg_usage->log_num);
    for(i=0;i<seg_usage->log_num;i++){
        lh = (struct log_header*)(buff + offset);
        HLOG_DEBUG("log_idx:%d", i);
        //      int idx = (seg_usage->log_num -1 )/8 +1;
        int idx = i / 8;
        HLOG_DEBUG("bitmap:%hhx\n,bit:%d is %hhx", seg_usage->bitmap[idx],i,seg_usage->bitmap[idx]&(i<<(i%8)));
        if((seg_usage->bitmap[idx] & (1<<(i%8)))){
            HLOG_DEBUG("copy! ....... ");
            HLOG_DEBUG("this log[%d]  need move ",i);
            HLOG_DEBUG("lh->start_db_no:%llu, lh->start_db_num: %d, lh->ib_num: %d log_size: %d",
			    				lh->start_db_no, lh->db_num, lh->ib_num, lh->log_size);
            HLOG_DEBUG("+: %llu", lh->start_db_no + lh->db_num - 1);
            uint32_t db_start = lh->start_db_no;
            uint32_t db_end = lh->start_db_no + lh->db_num - 1;
            HLOG_DEBUG("lh->start_db_no:%d, lh->end_d: %d", db_start, db_end);
            HLOG_DEBUG("append begin");
#if 1
            guint32 BLOCKSIZE = ctrl->sb.block_size;
            int expand_size =  (db_end-db_start + 1)*BLOCKSIZE + 
                ib_amount(db_start,db_end) * BLOCKSIZE + 
                LOG_HEADER_LENGTH + 
                sizeof(struct inode) + 
                sizeof(struct inode_map_entry);
            if (expand_size > ctrl->sb.seg_size) {
                HLOG_ERROR("write length is beyond the limit length!");
                return -1;
            }
            if (ctrl->last_offset + expand_size > ctrl->sb.seg_size) {
                ctrl->last_segno++;
                ctrl->last_offset = 0;
            }
            HLOG_DEBUG("last segno:%u last offset:%u", ctrl->last_segno,ctrl->last_offset);
#endif
            ret = append_log(ctrl,lh->data,(uint32_t) db_start, (uint32_t) db_end);
            HLOG_DEBUG("append over ret: %d", ret);
            if(-1 == ret) {
		    HLOG_ERROR("append_log error");
                goto out;
            }
        }else{
            HLOG_DEBUG("do not need to copy! ....... ");
        }
        ctrl->last_offset += ret;
        offset += lh->log_size;
    }
out:
    HLOG_DEBUG("leave func %s",__func__);
    g_free(buff);
    return ret;
}
