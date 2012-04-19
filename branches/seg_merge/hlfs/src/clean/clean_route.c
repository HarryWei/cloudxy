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
#include "segment_cleaner.h"

int rewrite_alive_blocks_from_seg(struct hlfs_ctrl *ctrl,struct segment_usage *seg_usage){
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
