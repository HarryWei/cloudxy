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

int seg_clean_task(struct hlfs_ctrl * ctrl)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (NULL == ctrl) {
		HLOG_ERROR("hlfs_ctrl input parameter error");
		return -1;
	}
    gint ret = 0;
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    GList * seg_usage_list = NULL;
    //GTimeVal expired;
    SEG_USAGE_T * seg_usage = NULL;
	int seg_idx = 0;
    while(ctrl->seg_clean_run){
        //g_get_current_time(&expired);
        //g_time_val_add(&expired,1000*1000*5);
        //g_usleep(1000*1000*5);
        if(get_current_time() - ctrl->last_access_timestamp < 1000*1000*5){
		   g_usleep(1000*1000);
		   continue;
        }
		if(seg_idx = -1){ //idx归-1则标识一轮结束
				 HLOG_DEBUG(" this rond check over ");
				 if(seg_usage_hashtable !=NULL){
				   g_hash_table_destroy  (seg_usage_hashtable);
				   seg_usage_hashtable = NULL;
				 }
				 if(seg_usage_list!=NULL){
				   g_queue_free(seg_usage_list); 
				   seg_usage_list = NULL;
				 }
				 HLOG_DEBUG(" next rond check kick off ");
				 ret = load_all_seg_usage(ctrl->storage,SEGMENTS_USAGE_FILE,seg_usage_hashtable);
				 g_assert(ret == 0);
				 seg_usage_list = g_hash_table_get_values(seg_usage_hashtable);
				 ret = sort_all_seg_usage(seg_usage_hashtable,seg_usage_list); /*安段号排序*/
				 seg_idx = 0;
				 HLOG_DEBUG(" seg usage count:%d ",g_list_length(seg_usage_list));
		}
		do{
			   seg_usage = (SEG_USAGE_T *)g_list_nth_data(seg_usage_list,seg_idx);
			   if(strlen(seg_usage->up_sname) > 0){
			   	  HLOG_DEBUG(" seg usage:%d is in snapshots ",seg_usage->segno);
				  /*是在快照区间,不进行回收*/
				  seg_idx++;
				  continue;
			   }else if(seg_usage->alive_block_num > g_atomic_int_get(&ctrl->ctrl_region->copy_waterlevel)){
			      HLOG_DEBUG(" seg usage:%d is beyond snapshots but alive block num:%d not reach waterlevel:%d ",
				  		       seg_usage->segno,
				  		       seg_usage->alive_block_num,
				  		       g_atomic_int_get(&ctrl->ctrl_region->copy_waterlevel));
				  /*未到阀值,则不进行回收*/
				  seg_idx++;
				  continue;
			   }else{
			      HLOG_DEBUG(" seg usage:%d is beyond snapshots and alive block num:%d  reach waterlevel:%d",
				  	           seg_usage->segno,
				  		       seg_usage->alive_block_num,
				  		       g_atomic_int_get(&ctrl->ctrl_region->copy_waterlevel));
				  break;
			   }
		}while(seg_idx <=  g_list_length(seg_usage_list)); //找到需要可进行强制回收的段
		
		if(seg_idx == g_list_length(seg_usage_list)){
			     HLOG_DEBUG(" check all seg usage over ! seg_idx:%d",seg_idx);
				 seg_idx = -1;
				 continue;
		}
		ret = migrate_alive_blocks(ctrl,seg_usage);
		g_assert(ret == 0);
		ret = dump_seg_usage(ctrl->storage,SEGMENTS_USAGE_FILE,seg_usage->segno);
		g_free(seg_usage->bitmap);
		g_free(seg_usage);
		g_assert(ret == 0);
		seg_idx++; //*每回收一个之后就要去重新检查是否有用户写请求
    }
	
    if(seg_usage_list!=NULL){
        g_list_free(seg_usage_list);
    }
	if(seg_usage_hashtable!=NULL){
    	g_hash_table_destroy(seg_usage_hashtable);
	}
    HLOG_DEBUG("leave func %s", __func__);
    return 0;
}
