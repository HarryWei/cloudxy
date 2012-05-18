#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage.h"
#include "storage_helper.h"
#include "bs_local.h"
#include "seg_clean.h"
#include "logger.h"

CTRL_REGION_T CTRL_REGION;
extern int append_log(struct hlfs_ctrl * ctrl,const char*db_buff,uint32_t db_start,uint32_t db_end);
int flush_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end){
    if ((NULL == ctrl) || (NULL == db_buff) ||db_end < db_start) {
		HLOG_ERROR("Params Error");
		return -1;
    }
    //HLOG_DEBUG("enter func %s", __func__);
    //HLOG_DEBUG("last segno: %u last offset: %u", ctrl->last_segno, ctrl->last_offset);
    g_mutex_lock (ctrl->hlfs_access_mutex);
    ctrl->last_write_timestamp = get_current_time();
    int size = append_log(ctrl,db_buff,db_start,db_end);
    g_mutex_unlock (ctrl->hlfs_access_mutex);
    if(size < 0){
        HLOG_ERROR("append log error");
        return -1;
    }
    HLOG_TRACE("Append Log Exec Succ . filesize:%llu,last_segno:%u, last_offset:%u,log size:%u,start_dbno:%u,end_dbno:%u",
	 	                                                ctrl->inode.length,
		                                                ctrl->last_segno,
		                                                ctrl->last_offset,
		                                                size,
		                                                db_start,
		                                                db_end)
   //HLOG_DEBUG("last offset: %u", ctrl->last_offset);
    return size;
}


int read_fs_superblock(struct back_storage *storage,struct super_block *sb)
{
	//HLOG_DEBUG("enter func %s", __func__);
	if ((NULL == storage) || (NULL == sb)) {
		HLOG_ERROR("read fs superblock error");
		return -1;
	}
    int ret = read_fs_meta(storage,&(sb->seg_size),&(sb->block_size),&(sb->max_fs_size));
    g_strlcpy(sb->fsname,g_basename(storage->uri),MAX_FILE_NAME_LEN);
    //HLOG_DEBUG("leave func %s", __func__);
    return ret;
}

/*
 * init_hlfs - initial the hlfs
 * @param uri fs localtion
 * @return Return a handle to the lhdfs
 */
struct hlfs_ctrl *
__init_hlfs(const char *uri, uint32_t is_clean_start ,uint32_t seg_clean_check_period,uint32_t copy_waterlevel,)
{
	//HLOG_DEBUG("enter func %s", __func__);
    if(uri == NULL || seg_clean_check_period == 0){
	  HLOG_ERROR("Params Error");
         return NULL;  
    }
    g_thread_init(NULL);
    struct hlfs_ctrl *ctrl = (struct hlfs_ctrl*)g_malloc0(sizeof(struct hlfs_ctrl));
    if (NULL == ctrl) {
	    HLOG_ERROR("ctrl allocate error!");
	    return NULL;
    }
    //ctrl->write_req_aqueue = g_async_queue_new();
    //ctrl->write_rsp_aqueue = g_async_queue_new();

    //HLOG_DEBUG("uri %s", uri);
    struct back_storage *storage = init_storage_handler(uri);
    if( storage == NULL){
        HLOG_ERROR("[uri:%s] can not accessable", uri);
        g_free(ctrl);
        return NULL;
    }
    //HLOG_DEBUG("storage name:%s,uri %s\n", (char *) storage->storage_name,storage->uri);
    ctrl->storage = storage;
    if(0!= read_fs_superblock(ctrl->storage,&ctrl->sb)){
            HLOG_ERROR("[uri:%s] read superblock failed",uri);
            g_free(ctrl);
            ctrl = NULL;
            goto out;
    }

    //HLOG_DEBUG("superblock read over\n");
    uint32_t segno=0;
    uint32_t offset = 0;

    if(0 != get_cur_latest_segment_info(ctrl->storage,&segno,&offset)){
        g_free(ctrl);
        ctrl = NULL;
        goto out;
    }

    ctrl->usage_ref = 0;
    ctrl->seg_clean_run = 1;
    memset(ctrl->alive_ss_name, 0, MAX_FILE_NAME_LEN);
    GThread * seg_clean_thread = g_thread_create((GThreadFunc)seg_clean_task,ctrl,TRUE,NULL);
    ctrl->seg_clean_thread = seg_clean_thread;
    ctrl->ctrl_region = &CTRL_REGION;
    ctrl->ctrl_region->is_start_clean      = is_clean_start;
    ctrl->ctrl_region->copy_waterlevel  = copy_waterlevel;
    ctrl->hlfs_access_mutex = g_mutex_new();
    ctrl->last_segno = segno;
    ctrl->last_offset = offset;
    if(ctrl->last_segno != 0 || ctrl->last_offset != 0){
        if( 0 != load_latest_inode_map_entry(ctrl->storage,ctrl->last_segno,ctrl->last_offset,&ctrl->imap_entry)){
            HLOG_ERROR("load inode map entry failed");
	     g_free(ctrl);
	     ctrl = NULL;
            goto out;
        }
    }
    ctrl->io_nonactive_period = seg_clean_check_period;
    HLOG_INFO("Raw Hlfs Ctrl Init Over ! uri:%s,max_fs_size:%llu,seg_size:%u,block_size:%u,last_segno:%u,last_offset:%u,io_nonactive_period:%u",
			    uri,
			    ctrl->sb.max_fs_size,
			    ctrl->sb.seg_size,
			    ctrl->sb.block_size,
			    ctrl->last_segno,
			    ctrl->last_offset,
			    ctrl->io_nonactive_period); 
out:
	//HLOG_DEBUG("leave func %s", __func__);
    return ctrl;
} 


struct hlfs_ctrl *
init_hlfs(const char *uri )
{      
        if(NULL != uri){
	     HLOG_ERROR("Params Error");	
	     return NULL;
        }		
        int ret = 0;
        uint32_t seg_clean_check_period = DEF_IO_NONACTIVE_PERIOD;
	 uint32_t seg_copy_waterlevel       = DEF_SEG_COPY_WATERLEVEL;
	 struct hlfs_ctrl * hlfs_ctrl = __init_hlfs(uri,1, seg_copy_waterlevel,seg_clean_check_period);
        if(NULL == hlfs_ctrl){
		 HLOG_ERROR("init raw hlfs ctrl failed");
		 return NULL;
        }			
	 uint32_t block_size,cache_size,flush_interval,flush_trigger_level,flush_once_size;
        block_size 		 	= hlfs_ctrl->sb.block_size;
        cache_size  		 	=  DEF_CACHE_SIZE;
        flush_interval 	 	=  DEF_FLUSH_INTERVAL;
        flush_trigger_level		=  DEF_FLUSH_TRIGGER_LEVEL;
        flush_once_size 	 	=  DEF_FLUSH_ONCE_SIZE;
        /* check .... */
        if(block_size!=hlfs_ctrl->sb.block_size){
              HLOG_ERROR("cache block size is not equal to block size in superblock"); 
              goto out;
           }
        if(flush_trigger_level > 100){
              HLOG_ERROR("cache flush_trigger_level can not > 100"); 
              goto out;
           }
        if(flush_once_size * block_size * 64 > hlfs_ctrl->sb.seg_size){
              HLOG_ERROR("flush_once_size can not too much:%llu",flush_once_size); 
              goto out;
           }

        hlfs_ctrl->cctrl = cache_new();
        ret = cache_init(hlfs_ctrl->cctrl,block_size,cache_size,flush_interval,flush_trigger_level,flush_once_size);
        if (ret !=0){
	       HLOG_ERROR("init cache failed");
              g_free(hlfs_ctrl->cctrl);
              hlfs_ctrl->cctrl=NULL;
              goto out;
        }
        cache_set_write_cb(hlfs_ctrl->cctrl,flush_log,hlfs_ctrl);
        HLOG_INFO("Data Block Cache Init Over ! cache_size:%u,block_size:%u,flush_interval:%u,flush_trigger_level:%u,flush_once_size:%d",
			       cache_size,block_size,flush_interval,flush_trigger_level,flush_once_size); 
        uint32_t iblock_size,icache_size,invalidate_trigger_level,invalidate_once_size;
        iblock_size =  hlfs_ctrl->sb.block_size;
        icache_size = DEF_ICACHE_SIZE;
        invalidate_trigger_level = DEF_INVALIDATE_TRIGGER_LEVEL;
        invalidate_once_size = DEF_INVALIDATE_ONCE_SIZE;
           /* check .... */
        if(iblock_size!=hlfs_ctrl->sb.block_size){
              HLOG_ERROR("cache block size is not equal to block size in superblock"); 
              goto out;
           }
        if(invalidate_once_size > 100){
              HLOG_ERROR("cache flush_trigger_level can not > 100"); 
              goto out;
           }
        if(invalidate_once_size * iblock_size * 64 > hlfs_ctrl->sb.seg_size){
              HLOG_ERROR("flush_once_size can not too much:%llu",invalidate_once_size); 
              goto out;
        }

        hlfs_ctrl->icache = icache_new();
        ret = icache_init(hlfs_ctrl->icache,iblock_size,icache_size,invalidate_trigger_level,invalidate_once_size);
        if (ret !=0){
	       HLOG_ERROR("init cache failed");
              g_free(hlfs_ctrl->icache);
              hlfs_ctrl->icache=NULL;
              goto out;
        }
	 HLOG_INFO("Indirect  Block Cache Init Over ! icache_size:%u,iblock_size:%u,invalidate_trigger_level:%u,invalidate_once_size:%u",
			       icache_size,iblock_size,invalidate_trigger_level,invalidate_once_size); 
	 return hlfs_ctrl;
out:
     deinit_hlfs(hlfs_ctrl);
     return NULL;	
} 

struct hlfs_ctrl *
init_hlfs_by_config(const char *config_file_path){
  if(NULL != config_file_path){
	     HLOG_ERROR("Params Error");	
	     return NULL;
   }		
   //HLOG_DEBUG("enter func %s", __func__);
   int ret = 0;
   GKeyFile * hlfs_conf_keyfile = g_key_file_new();
   
   //HLOG_DEBUG("config path:%s",config_file_path);
   gboolean res = g_key_file_load_from_file (hlfs_conf_keyfile,config_file_path,G_KEY_FILE_NONE,NULL);
   if(res == FALSE){
   	 g_key_file_free (hlfs_conf_keyfile);
        HLOG_ERROR("parse config file error", __func__);
        return NULL;
   }
   if (FALSE == g_key_file_has_group(hlfs_conf_keyfile,"STORAGE")){
   	 g_key_file_free (hlfs_conf_keyfile);
	 HLOG_ERROR("not find STORAGE option");
        return NULL;
   }
   
   const char * uri = g_key_file_get_string (hlfs_conf_keyfile,"STORAGE","storage_uri",NULL);
   
   uint64_t seg_clean_check_period = g_key_file_get_uint64 (hlfs_conf_keyfile,"STORAGE","seg_clean_check_period",NULL);
   if(seg_clean_check_period == 0){
   	   seg_clean_check_period = DEF_IO_NONACTIVE_PERIOD;
   }

   uint64_t seg_copy_waterlevel = g_key_file_get_uint64 (hlfs_conf_keyfile,"STORAGE","seg_copy_waterlevel",NULL);
   if(seg_copy_waterlevel == 0){
   	   seg_clean_check_period = DEF_SEG_COPY_WATERLEVEL;
   }

   gboolean _is_start_clean = g_key_file_get_boolean (hlfs_conf_keyfile,"STORAGE","is_start_clean",NULL);
   int is_start_clean;
   if(_is_start_clean == FALSE){
   	   is_start_clean = 0;
   }else{
          is_start_clean = 1;
   }
   
   struct hlfs_ctrl * hlfs_ctrl = __init_hlfs(uri,is_start_clean,seg_clean_check_period,seg_copy_waterlevel);
   if(hlfs_ctrl == NULL){
	  HLOG_ERROR("init raw hlfs failed");
	  g_key_file_free (hlfs_conf_keyfile);
         return NULL;
   }
   
   if (TRUE == g_key_file_has_group(hlfs_conf_keyfile,"CACHE")){
       //gsize length=0;
       //gchar * keys = g_key_file_get_keys(hlfs_conf_keyfile,"CACHE",&length,NULL);
       gboolean enable = g_key_file_get_boolean (hlfs_conf_keyfile,"CACHE","is_enable_cache",NULL);
       //HLOG_DEBUG("enable is :%d",enable); 
       if(TRUE ==  enable){
           HLOG_DEBUG("We Do Support Data Data Cache !!!"); 
           uint32_t block_size,cache_size,flush_interval,flush_trigger_level,flush_once_size;
           block_size 		= g_key_file_get_uint64 (hlfs_conf_keyfile,"CACHE","block_size",NULL);
           cache_size 		= g_key_file_get_uint64 (hlfs_conf_keyfile,"CACHE","cache_size",NULL);
           flush_interval 	= g_key_file_get_uint64(hlfs_conf_keyfile,"CACHE","flush_interval",NULL);
           flush_trigger_level = g_key_file_get_uint64 (hlfs_conf_keyfile,"CACHE","flush_trigger_level",NULL);
           flush_once_size 	   = g_key_file_get_uint64 (hlfs_conf_keyfile,"CACHE","flush_once_size",NULL);
           /* check .... */
           if(block_size!=hlfs_ctrl->sb.block_size){
              	HLOG_ERROR("cache block size is not equal to block size in superblock"); 
              	goto out;
           }
           if(flush_trigger_level > 100){
              	HLOG_ERROR("cache flush_trigger_level can not > 100"); 
              	goto out;
           }
           if(flush_once_size * block_size * 64 > hlfs_ctrl->sb.seg_size){
              	HLOG_ERROR("flush_once_size can not too much:%llu",flush_once_size); 
              	goto out;
           }

           hlfs_ctrl->cctrl = cache_new();
           ret = cache_init(hlfs_ctrl->cctrl,block_size,cache_size,flush_interval,flush_trigger_level,flush_once_size);
           if (ret !=0){
	          HLOG_ERROR("init cache failed");
                 g_free(hlfs_ctrl->cctrl);
                 hlfs_ctrl->cctrl=NULL;
                 goto out;
           }
           cache_set_write_cb(hlfs_ctrl->cctrl,flush_log,hlfs_ctrl);
	    HLOG_INFO("Data Block Cache Init Over ! cache_size:%u,block_size:%u,flush_interval:%u,flush_trigger_level:%u,flush_once_size:%d",
			       cache_size,block_size,flush_interval,flush_trigger_level,flush_once_size); 
       }
	 
   }
      if (TRUE == g_key_file_has_group(hlfs_conf_keyfile,"ICACHE")){
       //gsize length=0;
       //gchar * keys = g_key_file_get_keys(hlfs_conf_keyfile,"CACHE",&length,NULL);
       gboolean enable = g_key_file_get_boolean (hlfs_conf_keyfile,"ICACHE","is_enable_icache",NULL);
       //HLOG_DEBUG("enable is :%d",enable); 
       if(TRUE ==  enable){
           HLOG_DEBUG("do support cache!"); 
           uint64_t iblock_size,icache_size,invalidate_trigger_level,invalidate_once_size;
           iblock_size = g_key_file_get_uint64 (hlfs_conf_keyfile,"ICACHE","iblock_size",NULL);
           icache_size = g_key_file_get_uint64 (hlfs_conf_keyfile,"ICACHE","icache_size",NULL);
           invalidate_trigger_level = g_key_file_get_uint64 (hlfs_conf_keyfile,"ICACHE","invalidate_trigger_level",NULL);
           invalidate_once_size = g_key_file_get_uint64 (hlfs_conf_keyfile,"ICACHE","invalidate_once_size",NULL);
           /* check .... */
           if(iblock_size!=hlfs_ctrl->sb.block_size){
              HLOG_ERROR("cache block size is not equal to block size in superblock"); 
              goto out;
           }
           if(invalidate_once_size > 100){
              HLOG_ERROR("cache flush_trigger_level can not > 100"); 
              goto out;
           }
           if(invalidate_once_size * iblock_size * 64 > hlfs_ctrl->sb.seg_size){
              HLOG_ERROR("flush_once_size can not too much:%llu",invalidate_once_size); 
              goto out;
           }

           hlfs_ctrl->icache = icache_new();
           ret = icache_init(hlfs_ctrl->icache,iblock_size,icache_size,invalidate_trigger_level,invalidate_once_size);
           if (ret !=0){
	          HLOG_ERROR("init cache failed");
                 g_free(hlfs_ctrl->icache);
                 hlfs_ctrl->icache=NULL;
                 goto out;
           }
           HLOG_INFO("Indirect  Block Cache Init Over ! icache_size:%u,iblock_size:%u,invalidate_trigger_level:%u,invalidate_once_size:%u",
			       icache_size,iblock_size,invalidate_trigger_level,invalidate_once_size); 
       }
   }   
   g_key_file_free (hlfs_conf_keyfile);
   return hlfs_ctrl;
out:
   deinit_hlfs(hlfs_ctrl);
   return NULL;
}
