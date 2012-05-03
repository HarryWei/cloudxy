#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <stdlib.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage_helper.h"
#include "storage.h"
#include "snapshot.h"
#include "seg_clean.h"

int seg_usage2text(SEG_USAGE_T * seg_usage,char *textbuf){
       HLOG_DEBUG("enter func %s",__func__);
       memset(textbuf,0,sizeof(SEG_USAGE_T)*10);
       int n = sprintf (textbuf,"%llu %s %llu %llu %u %u %u",
		       seg_usage->segno,
		       seg_usage->up_sname,
		       seg_usage->inode_saddr,
		       seg_usage->timestamp,
		       seg_usage->log_num,
		       seg_usage->block_num,
		       seg_usage->alive_block_num);
       HLOG_DEBUG("textbuf init:%s, n:%d",textbuf,n);
	   int m=0;
       int i,j;
	   for(i=0;i<((seg_usage->log_num-1)/8)+1;i++){
           //for(j=0;j<8;j++){
	           //gboolean value = (segment_usage->bitmap[i] & (1<<(8 - j))) != 0;
	           //m += sprintf(textbuf + n + m,"%x ",value);
               //if(value==FALSE)
               //   printf("value:%d , offset m:%d\n",value,m);
           //}
           m += sprintf(textbuf+n+m,"%hhx#",seg_usage->bitmap[i]);
	   }
       sprintf(textbuf+n+m,"\n");
       HLOG_DEBUG("leave func %s",__func__);
	   return n+m+1;
}



int dump_seg_usage_text(struct back_storage * storage,const char*segment_usage_file, const char *seg_usage_text){
    HLOG_DEBUG("enter func %s",__func__);
    HLOG_DEBUG("enter func %s,seg usage file:%s",__func__,segment_usage_file);
    int ret = 0;
    uint32_t len;
    /*  TODO -- do not create runtime */
    bs_file_t file = NULL;
    if(-1 == storage->bs_file_is_exist(storage,segment_usage_file)){
        g_message("segment usage file not exist, create it");
        file = storage->bs_file_create(storage,segment_usage_file); 
        if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segment_usage_file);
            goto out; 
        }
        storage->bs_file_close(storage,file);
    }
    
    file = storage->bs_file_open(storage,segment_usage_file,BS_WRITEABLE);
    if(file == NULL){
        HLOG_ERROR("can not open segment usage file %s",segment_usage_file);
        goto out;
    }
    len = strlen(seg_usage_text);
    HLOG_DEBUG("segtextbuf is %s",seg_usage_text);
    if(0 > (ret = storage->bs_file_append(storage,file,seg_usage_text,len))){
        HLOG_ERROR("write seg usage faied:%d",ret);
        ret = -1;
        goto out;
    }
out:
    if(file!=NULL)
      storage->bs_file_close(storage,file);
       HLOG_DEBUG("leave func %s",__func__);
	return ret;
  
}

#if 0
int segment_delmark2text(uint32_t segno,char *textbuf){
       HLOG_DEBUG("enter func %s",__func__);
       uint32_t n = sprintf(textbuf,"%u\n",segno);
       HLOG_DEBUG("leave func %s",__func__);
	   return n;
}
int dump_segment_delmark(struct back_storage* storage,const char* segment_delmark_file,uint32_t segno){
    /*  TODO -- do not create runtime */
       HLOG_DEBUG("enter func %s",__func__);
    HLOG_DEBUG( "enter func %s,seg del mark file:%s",__func__,segment_delmark_file);
    int ret = 0;
    char segtextbuf[128];
    uint32_t len;
    bs_file_t file = NULL;
    if(-1 == storage->bs_file_is_exist(storage,segment_delmark_file)){
        g_message("segment usage file not exist, create it");
        file = storage->bs_file_create(storage,segment_delmark_file); 
        if(file==NULL){
            HLOG_ERROR("can not open segment file %s",segment_delmark_file);
            goto out; 
        }
        storage->bs_file_close(storage,file);
    }
    file = storage->bs_file_open(storage,segment_delmark_file,BS_WRITEABLE);
    if(file == NULL){
        HLOG_ERROR("can not open segment usage file %s",segment_delmark_file);
        goto out;
    }

    memset(segtextbuf,0,128);
    len = segment_delmark2text(segno,segtextbuf);
    HLOG_DEBUG("segtextbuf is %s",segtextbuf);
    if(0 > (ret = storage->bs_file_append(storage,file,segtextbuf,len))){
        HLOG_ERROR("write seg usage faied:%d",ret);
        ret = -1;
    }
out:
    if(file!=NULL)
        storage->bs_file_close(storage,file);
       HLOG_DEBUG("leave func %s",__func__);
    return ret;
}

#endif 



int load_seg_usage_from_text(struct back_storage *storage,SEG_USAGE_T * seg_usage, const char *textbuf){
     HLOG_DEBUG("enter func %s",__func__);
     HLOG_DEBUG("textbuf :%s",textbuf);
     gchar **v = g_strsplit (textbuf," ",1024);
     gchar *_segno_str = v[0]; 
	 gchar *_up_sname = v[1];
	 gchar *_inode_saddr_str = v[2]; 
	 gchar *_timestamp_str = v[3];
	 gchar *_log_num_str = v[4];
	 gchar *_block_num_str = v[5];
     gchar *_alive_block_str = v[6]; 
     gchar *bitmap_str = g_strdup(v[7]);
     HLOG_DEBUG("segno str:%s,up sname:%s,inode addr:%s,log num:%s,block num:%s,alive block str:%s",
	 		     _segno_str,
	 		     _up_sname,
	 		     _inode_saddr_str,
	 		     _log_num_str,
	 		     _block_num_str,
	 		     _alive_block_str);

     char *endptr = NULL;
     seg_usage->segno = strtoull(_segno_str,&endptr,0);
	 strcpy(seg_usage->up_sname,_up_sname,strlen(_up_sname));
	 seg_usage->inode_saddr = strtoull(_inode_saddr_str,&endptr,0);
	 seg_usage->timestamp = strtoull(_timestamp_str,&endptr,0);
	 seg_usage->log_num = strtoull(_log_num_str,&endptr,0);
     seg_usage->alive_blocks = strtoull(_alive_block_str,&endptr,0);
	 
     g_strfreev(v);
     HLOG_DEBUG("segno:%llu,alive_blocks:%llu,timestamp:%llu,log_num:%llu",
		         seg_usage->segno,seg_usage->alive_block_num,seg_usage->timestamp,seg_usage->log_num);
     seg_usage->bitmap = (char*)g_malloc0((seg_usage->log_num - 1)/8 +1);
     HLOG_DEBUG("bitmap_str :%s",bitmap_str);
     v = g_strsplit(bitmap_str,"#",1024);
     int i;
     for(i=0;i<g_strv_length(v) - 1;i++){
         seg_usage->bitmap[i] = strtoul(v[i],&endptr,16);
     }
     g_strfreev(v);
       HLOG_DEBUG("leave func %s",__func__);
     return 0;
}


//TODO list  


/*该函数用于根据段号和快照表找到回收使用的参考inode*/
int get_refer_inode(struct back_storage *storage,uint64_t segno,GHashTable* seg_usage_hashtable){
    int num_entries;
	struct snapshot* snapshots = __hlfs_get_all_snapshots(storage,&num_entries);

	#if 0
	   g_slist_sort(segno，snapshot_hashtable,compare_func); /*按照段号，即时间序排序*/
	   for_each_item_in_snapshots{
		   if (get_segno(prev_snapshot->inode_addr) < segno) && (get_segno(cur_snapshot->inode_addr) > segno){
			   /* 在快照区间内,获取up snapshot的inode地址；注意如果只有1个快照，区间是段0到当前快照所在段区间，这个要单独处理*/
			   inode = load_inode(cur_snapshot->inode_addr);
		   }
		   pre_snapshot=cur_snapshot;
	   }
	   inode = semi_latest_inode(); 对于非快照区间的段,获取次新inode地址；
	   return inode;
	#endif
    return 0;
}




