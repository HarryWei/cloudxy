/*
 * @author kanghua(kanghua151@msn.com) 
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "logger.h"

#if 1 /* 0*/ 
static int create_new_segfile(void *storage_handler, uint32_t segno) 
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    gchar * path = (gchar*)storage_handler;
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(segno,segfile_name);
    char *segfile_path = g_build_filename(path, segfile_name, NULL);
    if (-1 == g_creat(segfile_path,0700)) {
	    HLOG_ERROR("create segfile error");
        ret = -1;
        goto out;
    }
out:
    g_free(segfile_path);
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}
#endif 

int hlfs_write(struct hlfs_ctrl *ctrl, char *write_buf, uint32_t write_len, uint64_t pos)
{
    HLOG_DEBUG("enter func %s", __func__);
    if ((NULL == ctrl) || (NULL == write_buf) || (0 == write_len) || (ctrl->sb.seg_size < write_len)) {
		HLOG_ERROR("hlfs_write error");
		return -1;
    }
    g_mutex_lock (ctrl->hlfs_access_mutex);
    int ret = 0;
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    uint64_t cur_time;
    uint32_t db_start = pos/BLOCKSIZE;
    uint32_t db_end   = (pos+write_len-1)/BLOCKSIZE;
    int write_blocks_size = (db_end - db_start + 1 )*BLOCKSIZE; 
    char * datablocks = (char*)g_malloc0(write_blocks_size);
    if (!datablocks) {
            HLOG_ERROR("allocate error!");
            g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }

    HLOG_DEBUG("write offset:%llu,write len:%d", pos,write_len);
    if(db_start == db_end){
        HLOG_DEBUG("only need to write part in one block:%llu", pos / BLOCKSIZE);
        char *block=NULL;
        if(1==(ret=load_block_by_addr(ctrl,pos,&block))){
		HLOG_DEBUG("fail to load block for addr! %llu", pos);
            	block = (char*)g_malloc0(BLOCKSIZE);
            	if (!block) {
            		HLOG_ERROR("# -- allocate error!");
                	g_mutex_unlock (ctrl->hlfs_access_mutex);
                	return -1;
            	}
       }else if(-1 == ret){
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }
        memcpy(datablocks,block,pos%BLOCKSIZE);
        memcpy(datablocks + pos%BLOCKSIZE,write_buf,write_len);
        memcpy(datablocks + pos%BLOCKSIZE + write_len, 
               block + pos%BLOCKSIZE + write_len,
               BLOCKSIZE-(pos%BLOCKSIZE+write_len));
        g_free(block);
        goto write_log;
    }
    if(pos % BLOCKSIZE != 0 ){
        HLOG_DEBUG("to load first block!");
        char *first_block = NULL;
        if(1==(ret =load_block_by_addr(ctrl,pos,&first_block))){
            HLOG_DEBUG("fail to load first block");
            first_block = (char*)g_malloc0(BLOCKSIZE);
            if (!first_block) {
            	HLOG_ERROR("allocate error!");
                g_mutex_unlock(ctrl->hlfs_access_mutex);
                return -1;
            }
        }else if(ret == -1){
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }
        memcpy(datablocks,first_block,BLOCKSIZE);
        memcpy(datablocks+pos%BLOCKSIZE,write_buf,write_len);
        g_free(first_block);
    }else{
        HLOG_DEBUG("do not need load first block");
        memcpy(datablocks, write_buf, write_len);
    }
    if((pos +write_len)%BLOCKSIZE !=0){
        HLOG_DEBUG("to load last block");
        char *last_block = NULL;
        if(1==(ret=load_block_by_addr(ctrl, pos + write_len, &last_block))){
            HLOG_DEBUG("fail to load last block");
            last_block = (char*)g_malloc0(BLOCKSIZE);
            if (!last_block) {
            HLOG_ERROR("allocate error!");
                g_mutex_unlock (ctrl->hlfs_access_mutex);
                return -1;
            }
        }else if(-1==ret){
            g_mutex_unlock (ctrl->hlfs_access_mutex);
            HLOG_ERROR("can not read logic block: %llu", pos / BLOCKSIZE);
            return -1;
        }
        memcpy(datablocks+pos%BLOCKSIZE + write_len,
                last_block+(write_len+pos)%BLOCKSIZE,
                BLOCKSIZE-(write_len+pos)%BLOCKSIZE);
        g_free(last_block);
    }
write_log:;
	HLOG_DEBUG("db_start: %u db_end: %u", db_start, db_end);
	if(ctrl->inode.length < (pos + write_len)){ 
		ctrl->inode.length = pos + write_len;
	}
	cur_time = get_current_time();
	ctrl->inode.mtime  = cur_time;
	ctrl->inode.ctime  = cur_time;
	ctrl->inode.atime  = cur_time;
	g_message("get_current_time is %llu", ctrl->inode.mtime);
    int expand_size =  (db_end-db_start + 1)*BLOCKSIZE + 
    ib_amount(db_start,db_end) * BLOCKSIZE + 
    LOG_HEADER_LENGTH + 
    sizeof(struct inode) + 
    sizeof(struct inode_map_entry);
    if (expand_size > ctrl->sb.seg_size) {
		HLOG_ERROR("write length is beyond the limit length!");
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        return -1;
	}
	if (ctrl->last_offset + expand_size > ctrl->sb.seg_size) {
		ctrl->last_segno++;
		ctrl->last_offset = 0;
	}
	HLOG_DEBUG("last segno: %u last offset: %u", ctrl->last_segno, ctrl->last_offset);
#if 0
    int size = append_log(ctrl,datablocks,db_start,db_end);
    if(size < 0){
		g_message("fail to append log\n");
		return -1; 
	}
#else 
	struct write_req *w_req = &ctrl->write_req;
    //struct write_rsp *w_rsp = &ctrl->write_rsp;
    //struct write_req * w_req = (struct write_req*)g_malloc0(sizeof(struct write_req));
    w_req->req_buf = datablocks;
    w_req->db_start = db_start;
    w_req->db_end = db_end;
    g_async_queue_push(ctrl->write_req_aqueue,(gpointer)w_req);
    HLOG_DEBUG("request pushed to aysn");
    struct write_rsp * w_rsp = (struct write_rsp*)g_async_queue_pop(ctrl->write_rsp_aqueue);
    ret = w_rsp->res;
    int size = w_rsp->size;
    //g_free(w_rsp);
    if(ret!=0){
		HLOG_ERROR("fail to append log");
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        return -1;
	}
#endif 
    g_free(datablocks);
    ctrl->last_offset += size;
	HLOG_DEBUG("last offset: %u fs length: %lld", ctrl->last_offset, ctrl->inode.length);
    g_mutex_unlock (ctrl->hlfs_access_mutex);
	HLOG_DEBUG("leave func %s", __func__);
    return write_len;
}
