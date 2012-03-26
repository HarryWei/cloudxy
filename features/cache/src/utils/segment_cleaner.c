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
#include "segment_cleaner.h"

int segment_usage2text(const struct segment_usage* segment_usage,char *textbuf){
       HLOG_DEBUG("enter func %s",__func__);
       memset(textbuf,0,sizeof(struct segment_usage)*10);
       int n = sprintf (textbuf,"%llu %llu %llu %llu ",
		       segment_usage->segno,segment_usage->alive_blocks,
		       segment_usage->timestamp,segment_usage->log_num);
       HLOG_DEBUG("textbuf init:%s, n:%d",textbuf,n);
	   int m=0;
       int i,j;
	   for(i=0;i<((segment_usage->log_num-1)/8)+1;i++){
           //for(j=0;j<8;j++){
	           //gboolean value = (segment_usage->bitmap[i] & (1<<(8 - j))) != 0;
	           //m += sprintf(textbuf + n + m,"%x ",value);
               //if(value==FALSE)
               //   printf("value:%d , offset m:%d\n",value,m);
           //}
           m += sprintf(textbuf+n+m,"%hhx#",segment_usage->bitmap[i]);
	   }
       sprintf(textbuf+n+m,"\n");
       HLOG_DEBUG("leave func %s",__func__);
	   return n+m+1;
}

int dump_segment_usage_text(struct back_storage * storage,
		const char*segment_usage_file, 
		const char * seg_usage_text){
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

int dump_segment_usage(struct back_storage * storage,
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


int load_segment_usage_from_text(struct back_storage *storage,
		struct segment_usage * seg_usage, 
		const char *textbuf){
     HLOG_DEBUG("enter func %s",__func__);
     HLOG_DEBUG("textbuf :%s",textbuf);
     gchar **v = g_strsplit (textbuf," ",1024);
     gchar *_segno_str = v[0]; 
     gchar *_alive_block_str = v[1]; 
     gchar *_timestamp_str = v[2];
     gchar *_log_num_str = v[3];
     gchar *bitmap_str = g_strdup(v[4]);
     HLOG_DEBUG("segno str:%s,alive block str:%s",_segno_str,_alive_block_str);

     char *endptr = NULL;
     seg_usage->segno = strtoull(_segno_str,&endptr,0);
     seg_usage->alive_blocks = strtoull(_alive_block_str,&endptr,0);
     seg_usage->timestamp = strtoull(_timestamp_str,&endptr,0); 
     seg_usage->log_num = strtoull(_log_num_str,&endptr,0);
     g_strfreev(v);
     HLOG_DEBUG("segno:%llu,alive_blocks:%llu,timestamp:%llu,log_num:%llu",
		     seg_usage->segno,seg_usage->alive_blocks,seg_usage->timestamp,seg_usage->log_num);
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


int load_all_segment_usage(struct back_storage *storage,
			const char *seg_usage_file,
			const char * seg_delmark_file,
			GHashTable* seg_usage_hashtable){
       HLOG_DEBUG("enter func %s",__func__);
    bs_file_t file = storage->bs_file_open(storage,seg_usage_file,BS_READONLY);      
    if(NULL==file){
        HLOG_DEBUG(" open seg usage  file failed ! not exist ? ");
        return 0;
    }
    char textbuf[8192*10];
    memset(textbuf,0,8192*10);
    uint32_t count = storage->bs_file_pread(storage,file,textbuf,8192*10,0);
    if(count < 0){
        g_message(" read seg usage file failed ");
        storage->bs_file_close(storage,file);
        return -1;
    }

    HLOG_DEBUG("===read count %d ,%s",count,textbuf);
#if 1
    gchar ** segs = g_strsplit(textbuf,"\n",0);
    HLOG_DEBUG("there are %d segno in segment usage file",g_strv_length(segs));
    int i;
    for(i=0;i<g_strv_length(segs)-1;i++){
        struct segment_usage* seg_usage= (struct segment_usage*)g_malloc(sizeof(struct segment_usage));
        load_segment_usage_from_text(storage,seg_usage,segs[i]);
        //g_hash_table_insert(seg_usage_hashtable,seg_usage->segno,seg_usage);
        HLOG_DEBUG("segno:%llu",seg_usage->segno);
        g_hash_table_insert(seg_usage_hashtable,GINT_TO_POINTER((uint32_t) seg_usage->segno),seg_usage);
    }
    g_strfreev(segs);
#else 
    char *line = textbuf;
    int offset = 0;
    do{
      HLOG_DEBUG("........line:%s,len:%d",line,strlen(line));
      int len = strlen(line);
      line += len+1;
      offset += len+1 ;
    }while(offset < count);
#endif
    HLOG_DEBUG("read seg usage del mark file...");
    file = storage->bs_file_open(storage,seg_delmark_file,BS_READONLY);      
    if(NULL == file){
        HLOG_DEBUG(" open seg del mark file failed ! not exist ? ");
        return 0;
    }
    memset(textbuf,0,8192*10);
    count = storage->bs_file_pread(storage,file,textbuf,8192*10,0);
    HLOG_DEBUG("===read count %d ,%s",count,textbuf);
    storage->bs_file_close(storage,file);
    if(count < 0){
        HLOG_ERROR(" read seg del mark file failed ");
        return -1;
    }else if(count == 0){
        return 0;
    }
#if 1
    segs = g_strsplit(textbuf,"\n",0);
    char* endptr = NULL;
    for(i=0;i<g_strv_length (segs)-1;i++){
        HLOG_DEBUG("segment :%s has del",segs[i]);
        uint32_t segno = strtoul(segs[i],&endptr,0);
        g_hash_table_remove(seg_usage_hashtable,GINT_TO_POINTER(segno));
    }
    g_strfreev(segs);
#endif
    HLOG_DEBUG("read seg del mark file ...");
       HLOG_DEBUG("leave func %s",__func__);
    return 0;
}

int read_segment_usage(struct back_storage * storage,
		struct segment_usage* seg_usage,uint32_t segno){
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

int segment_usage_calc(struct back_storage* storage, const char *segfile,
			struct inode * latest_inode ,struct segment_usage *seg_usage,
			uint32_t block_size) 
{
    HLOG_DEBUG("enter func %s",__func__);
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
