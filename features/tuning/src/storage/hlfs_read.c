/*
 * @author kanghua(kanghua151@msn.com) 
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "logger.h"

/*
 * hlfs_read: Read data from file to memory.
 * @param ctrl: The global control structure.
 * @param read_buf: 
 */

int hlfs_read(struct hlfs_ctrl *ctrl, char* read_buf, uint32_t read_len, uint64_t pos)
{
	HLOG_DEBUG("enter func %s", __func__);
    if((NULL == read_buf) || (NULL == ctrl) || (0 == read_len)){
	HLOG_ERROR("hlfs_read error");
           return -1;
    }
    //g_mutex_lock (ctrl->hlfs_access_mutex);
    guint32 BLOCKSIZE = ctrl->sb.block_size;
    HLOG_DEBUG("read offset:%llu,read len:%d", pos,read_len);
    int ret = 0;
    int start_db = 0;
    if(pos/BLOCKSIZE == (pos+read_len-1)/BLOCKSIZE){
		HLOG_DEBUG("only need to read one block: %llu", pos / BLOCKSIZE);
        char *block = NULL;
        g_mutex_lock (ctrl->hlfs_access_mutex);
        ret=load_block_by_addr_fast(ctrl,pos,&block);
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(-1 == ret ){
			HLOG_ERROR("fail to load block for addr %llu", pos);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }else if(1==ret){
            block = (char*)g_malloc0(BLOCKSIZE);
            memset(block,0,BLOCKSIZE);
        }
        memcpy(read_buf,block + pos%BLOCKSIZE,read_len);
        g_free(block);
        //g_mutex_unlock (ctrl->hlfs_access_mutex);
        HLOG_DEBUG("read len %u", read_len);
        return read_len;
    }

    HLOG_DEBUG("need to read muti block", __func__);
    uint32_t offset=0; 
    if( pos % BLOCKSIZE != 0 ){
		HLOG_DEBUG("need to read first block", __func__);
        char *block = NULL;
        g_mutex_lock (ctrl->hlfs_access_mutex);
        ret=load_block_by_addr_fast(ctrl,pos,&block);
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(-1 == ret){
            HLOG_ERROR("fail to load block for addr %llu", pos);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }else if(1 == ret){
            block = (char*)g_malloc0(BLOCKSIZE);
            memset(block,0,BLOCKSIZE);
        }
        memcpy(read_buf,block + pos%BLOCKSIZE, BLOCKSIZE - pos%BLOCKSIZE);
        offset += BLOCKSIZE - pos%BLOCKSIZE;
        HLOG_DEBUG("fist offset:%u", offset);
        g_free(block);
        start_db = (pos + BLOCKSIZE)/BLOCKSIZE;
    }else{
        start_db = pos/BLOCKSIZE;
    }
    int end_db = (pos+read_len)/BLOCKSIZE;
    HLOG_DEBUG("start db: %d end db: %d", start_db, end_db);
    int i;
    for(i = start_db; i < end_db;i++){
        char *block = NULL;
        g_mutex_lock (ctrl->hlfs_access_mutex);
        ret=load_block_by_no_fast(ctrl,i,&block);
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(-1 == ret){
            HLOG_ERROR("fail to load block for no %d", i);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }else if(1==ret){
            block = (char*)g_malloc0(BLOCKSIZE);
            memset(block,0,BLOCKSIZE);
        }
        memcpy(read_buf+offset,block,BLOCKSIZE);
        offset +=BLOCKSIZE;
        HLOG_DEBUG("offset: %u", offset);
        g_free(block);
    }
    if((pos + read_len)% BLOCKSIZE != 0 ){
        HLOG_DEBUG("need to read last block", __func__);
        char *block = NULL;
        g_mutex_lock (ctrl->hlfs_access_mutex);
        ret=load_block_by_addr_fast(ctrl,pos+read_len,&block);
        g_mutex_unlock (ctrl->hlfs_access_mutex);
        if(-1 == ret){
            HLOG_ERROR("fail to load block for addr %llu", pos + read_len);
            //g_mutex_unlock (ctrl->hlfs_access_mutex);
            return -1;
        }else if (1==ret){
            block = (char*)g_malloc0(BLOCKSIZE);
            memset(block,0,BLOCKSIZE);
        }
        memcpy(read_buf + offset , block , (pos + read_len)%BLOCKSIZE );
        offset +=(pos+read_len)%BLOCKSIZE;
        HLOG_DEBUG("last offset: %u", offset);
        g_free(block);
    }
    //g_mutex_unlock (ctrl->hlfs_access_mutex);
    ctrl->last_access_timestamp = get_current_time();
    HLOG_DEBUG("read len %u", offset);
	HLOG_DEBUG("leave func %s", __func__);
    return offset;
}
