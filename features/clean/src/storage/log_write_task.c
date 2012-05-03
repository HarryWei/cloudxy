#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage.h"
#include "storage_helper.h"
#include "seg_clean.h"

int log_write_task(struct hlfs_ctrl * ctrl)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (NULL == ctrl) {
		HLOG_ERROR("hlfs_ctrl input parameter error");
		return -1;
	}
    gint seg_idx = 0;
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    GList * seg_usage_list = NULL;
    struct write_req *w_req = &ctrl->write_req;
    struct write_rsp *w_rsp = &ctrl->write_rsp;
    GTimeVal expired;
	seg_idx = 0;
    while(ctrl->write_task_run){
        g_get_current_time(&expired);
        g_time_val_add(&expired,1000*1000);
        w_req = (struct write_req*)g_async_queue_timed_pop(ctrl->write_req_aqueue,&expired);
        if(w_req != NULL){
            HLOG_DEBUG("real write request comming");
            int size = 0;
            if(ctrl->cctrl != NULL){
               HLOG_DEBUG("use write back mode");
               int ret = cache_insert_blocks(ctrl->cctrl,w_req->db_start,(w_req->db_end - w_req->db_start + 1),w_req->req_buf); g_assert(ret == 0);
               size = 0; 
            }else{
               HLOG_DEBUG("use write through mode");
               size = append_log(ctrl,w_req->req_buf,w_req->db_start,w_req->db_end);
            }
            //w_rsp = (struct write_rsp*)g_malloc0(sizeof(struct write_rsp));
            if(size < 0){
                HLOG_DEBUG("failed to append log");
                w_rsp->res = -1;
            }else{
                w_rsp->res = 0;
                w_rsp->size = size;
            }
            g_async_queue_push(ctrl->write_rsp_aqueue,(gpointer)w_rsp);
            //g_free(w_req);
            continue;
            /*  */
        }else{
        #if 0
            //HLOG_DEBUG("no real write request for expired ,do copy for cleaning");
            if(g_atomic_int_get(&ctrl->ctrl_region->is_start_clean) == 1){
                int ret = load_all_segment_usage(ctrl->storage,SEGMENTS_USAGE_FILE,SEGMENTS_DEL_FILE,seg_usage_hashtable);
                g_assert(ret == 0);
                if(seg_usage_list!=NULL)
                   g_list_free(seg_usage_list);
                if(g_hash_table_size(seg_usage_hashtable) == 0)
                   g_hash_table_destroy(seg_usage_hashtable);
                seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
            }else{
                //HLOG_DEBUG("no not need to do clean");
				seg_idx = 0;
                continue;
            }
            if(g_list_length(seg_usage_list) == 0){
                //HLOG_DEBUG("no seg usage");
				seg_idx = 0;
                continue;
            }
            if(seg_idx == g_list_length(seg_usage_list)){
                HLOG_DEBUG("check over !!!");
				seg_idx = 0;
                continue;
            }
            seg_usage_list = g_hash_table_get_values(seg_usage_hashtable);
            struct segment_usage *seg_usage = (struct segment_usage*)g_list_nth_data(seg_usage_list,seg_idx);
			seg_idx += 1;
            if(seg_usage->alive_blocks > g_atomic_int_get(&ctrl->ctrl_region->copy_waterlevel)){
                continue;
            }
            int ret = rewrite_alive_blocks_from_seg(ctrl,seg_usage);
            g_assert(ret != -1);
#ifdef SUPPORT_LOCAL_FILESYSTEM
            g_mutex_lock (ctrl->hlfs_access_mutex);
#endif
            dump_segment_delmark(ctrl->storage,SEGMENTS_DEL_FILE,seg_usage->segno);
#ifdef SUPPORT_LOCAL_FILESYSTEM
            g_mutex_unlock (ctrl->hlfs_access_mutex);
#endif
            const char segfile[SEGMENT_FILE_NAME_MAX];
            build_segfile_name(seg_usage->segno,segfile);
            ctrl->storage->bs_file_delete(ctrl->storage,segfile);
            g_free(seg_usage->bitmap);
            /*  */
#endif 
        }
    }
    if(seg_usage_list!=NULL)
        g_list_free(seg_usage_list);
    g_hash_table_destroy(seg_usage_hashtable);
    HLOG_DEBUG("leave func %s", __func__);
    return 0;
}
