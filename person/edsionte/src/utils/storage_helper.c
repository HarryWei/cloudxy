/*
  *  Copyright (C) 2012      KangHua <kanghua151@gmail.com> 
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <signal.h>
#include "glib.h"
#include "comm_define.h"
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "storage.h"
#include "address.h"
#include "misc.h"
#include "bs_local.h"
#include "bs_hdfs.h"

//struct back_storage* init_storage_handler(const char* uri,const char *fs_name)
struct back_storage* init_storage_handler(const char* uri)
{
    HLOG_DEBUG("enter func %s", __func__);
    int ret =0;
    struct back_storage* storage = NULL;
    gchar* back_storage_type = NULL;
    char *head=NULL;
    char *hostname=NULL;
    char *dir=NULL;
    char *fs_name = NULL;
    int port;
    ret = parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
    if(ret !=0){
	 HLOG_ERROR("parse_from_uri happened error");
        return NULL;
    }
    //gchar *fs_path = g_build_filename(dir,fs_name,NULL);
    HLOG_DEBUG("loc [fs:%s], \n",fs_name);
    if (0 == g_strcmp0(head,"local")){
        storage=get_local_storage_ops();
    }else if(0 == g_strcmp0(head,"hdfs")){
        storage = get_hdfs_storage_ops();
    }else{
        HLOG_ERROR("Error URI:%s",uri);
        ret = -1;
        goto out;
    }
       storage->uri = strdup(uri);
	storage->head = head;
	storage->dir = dir;
	storage->fs_name = fs_name;
	storage->hostname = hostname;
       storage->port = port;
	storage->user = g_strdup("kanghua");
    HLOG_DEBUG("uri:%s,head:%s,dir:%s,fsname:%s,hostname:%s,port:%d,user:%s",
                storage->uri,
                storage->head,
                storage->dir,
                storage->fs_name,
                storage->hostname,
                storage->port,
                storage->user);
    if(0!=storage->bs_fs_connect(storage,uri)){
        ret = -1;
        goto out;
        HLOG_ERROR("connect filesystem failed");
        return NULL;
    }
out:
    //g_free(fs_path);
    if(ret !=0){
		if(storage->uri != NULL)
			g_free(storage->uri);
		if(storage->head !=NULL)
	       	g_free(storage->head);
		if(storage->dir !=NULL);
			g_free(storage->dir);
		if(storage->fs_name !=NULL)
			g_free(storage->fs_name);
		if(storage->hostname !=NULL)
			g_free(storage->hostname);
		if(storage->user!=NULL)
			g_free(storage->user);
        	g_free(storage);
		HLOG_ERROR("ret is not 0, so error happened");
        storage = NULL;
    }
	HLOG_DEBUG("leave func %s", __func__);
    return storage;
}


int deinit_storage_handler(struct back_storage * storage){
	HLOG_DEBUG("enter func %s", __func__);
    if(storage == NULL){
       HLOG_ERROR("storage is NULL");
       return -1;
    }
    if(0!=storage->bs_fs_disconnect(storage)){
       return -1;
    }
    HLOG_DEBUG("disconnect succ");
    //g_free(storage->storage_name);
    //g_message("%s disconnect succ\n",__func__);
    //g_free(storage->fs_name);
    //g_free(storage->fs_location);
    //g_message("%s disconnect succ\n",__func__);
    //g_free(storage->user);
	HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

/*
 * get_last_inode_addr: get the segment's lastest log's inode address.
 * @para ctrl: the global control structure of HLFS.
 * @para segfile: the segment file name.
 * @return: if error happens return -1, if right return the inode address.
 */
uint64_t get_last_inode_storage_addr_in_seg( struct back_storage * storage, uint32_t segno) 
{
       HLOG_DEBUG("enter func %s",__func__);
#if 1
	bs_file_t file = NULL;
	bs_file_info_t *info = NULL;
	struct inode_map_entry im;
       const char segfile[SEGMENT_FILE_NAME_MAX];
       build_segfile_name(segno,segfile);
	if (0 == storage->bs_file_is_exist(storage, segfile)) {
		if (NULL == (file = storage->bs_file_open(storage, segfile, BS_READONLY))) {
			HLOG_ERROR("open segfile error!");
			return -1;
		}
		if (NULL == (info = storage->bs_file_info(storage, segfile))) {
			storage->bs_file_close(storage, file);
			HLOG_ERROR("get segfile info error!");
			return -1;
		}
		HLOG_DEBUG("segfile size: %lld", info->size);
		if (sizeof(struct inode_map_entry)  != storage->bs_file_pread(storage, file, 
							(char*)&im, sizeof(struct inode_map_entry) , 
							info->size - sizeof(struct inode_map_entry) )) {
			storage->bs_file_close(storage, file);
			HLOG_ERROR("pread error!");
			return -1;
		}
	} else {
		HLOG_ERROR("segfile not exist!");
		return -1;
	}
	storage->bs_file_close(storage, file);
	HLOG_DEBUG("im.inode_no:%lld,im.inode_addr:%lld(segno:%u,offset:%u)", 
			im.inode_no,im.inode_addr,get_segno(im.inode_addr),get_offset(im.inode_addr));
	HLOG_DEBUG("leave func %s", __func__);
	return im.inode_addr;
#endif
}

/*FIX IT ... */
static int restore_last_segno_file(const char* uri,const char*path){
	HLOG_DEBUG("enter func %s", __func__);
       int ret;
       const char* head,*hostname,*dir,*fs_name;
       int port;
	   HLOG_DEBUG("parse uri:%s",uri);
       ret = parse_from_uri(uri, (char **) &head, (char **) &hostname ,(char **) &dir, (char **) &fs_name, &port);
       if(ret !=0){
	       HLOG_DEBUG("parse uri error:%s",uri);
           return -1;
       }
	   struct sigaction sa,oldsa;
       sa.sa_handler = SIG_IGN;
       sigaction( SIGCHLD, &sa, &oldsa);
       //sigset_t mask,omask;
       //sigemptyset(&mask);
       //sigaddset(&mask,SIGCHLD);
       //if(sigprocmask(SIG_BLOCK,&mask,&omask)<0){
	   //    HLOG_DEBUG("SIGBLOCK ERROR");
       //    g_assert(0);
       //    return -1;
       //}
       const char full_segno_file_path[256];
       const char tmp_segno_file_path[256];
       const char *segfile = g_basename(path);
       sprintf((char *) full_segno_file_path,"%s/%s",uri,segfile);
       sprintf((char *) tmp_segno_file_path,"%s%s",full_segno_file_path,".bak");
       HLOG_DEBUG("full segno file path :%s",full_segno_file_path);
       const char cmd[256];
       pid_t status;
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s%s%s","hadoop fs"," -cp ",full_segno_file_path," ",tmp_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s","hadoop fs"," -rm ",full_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s%s%s","hadoop fs"," -mv ",tmp_segno_file_path," ",full_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       //sigprocmask(SIG_SETMASK,&omask,NULL);
       sigaction(SIGCHLD,&oldsa,0);
	   HLOG_DEBUG("leave func %s", __func__);
       return 0;
}

int get_cur_latest_segment_info(struct back_storage * storage,uint32_t *segno,uint32_t *offset){
    HLOG_DEBUG("enter func %s",__func__);
    int ret =0;
    uint32_t num_entries;
    bs_file_info_t * infos = storage->bs_file_list_dir(storage,".",&num_entries);
    if(infos == NULL){
       HLOG_ERROR("can not get fs:%s seg entries",storage->uri);
       return -1;
    }
    HLOG_DEBUG("how much file :%d\n",num_entries);
    int i=0;
    const char* latest_file = NULL;
    uint64_t latest_st_mtime = 0;
    bs_file_info_t * info = infos;
    bs_file_info_t * __info = NULL;
    for(i=0;i<num_entries;i++){
        HLOG_DEBUG("7777 file:%s,size:%llu,time:%llu\n",info->name,info->size,info->lmtime);  
        if(g_str_has_suffix(info->name,"seg")){
             if(latest_st_mtime < info->lmtime){
                latest_st_mtime = info->lmtime;
                latest_file = info->name;
				__info = info;
                //*offset = info->size;
#if 0
                if(info->size == 0){
			HLOG_DEBUG("meta data .................. not update ?");
			ret = restore_last_segno_file(storage->uri,info->name); 
			g_assert(0==ret);
			bs_file_info_t * _info = storage->bs_file_info(storage,info->name);
			*offset = _info->size;
			free(_info);
		}
#endif
	     }
	} 
	info++;
    }
    HLOG_DEBUG("7777 file:%s,size:%llu,time:%llu\n",info->name,info->size,info->lmtime);  

    if(latest_file!=NULL){
	    HLOG_DEBUG("latest file:%s",latest_file);
	    const gchar *basename = g_basename(latest_file);
	    gchar **v = g_strsplit(basename,".",2);
	    *segno = atol(v[0]);
	    HLOG_DEBUG("segno :%d",*segno);
	    g_strfreev(v);
#if 1
	    if (__info->size == 0) {
		    HLOG_DEBUG("meta data .................. not update ?");
		    ret = restore_last_segno_file(storage->uri,latest_file); 
		    g_assert(0==ret);
		    bs_file_info_t * _info = storage->bs_file_info(storage,latest_file);
		    *offset = _info->size;
		    free(_info);
	    }else{
              *offset = __info->size;
	    }
#endif
    }
out:
    HLOG_DEBUG("leave func %s", __func__);
    free(infos);
    return ret;   
}

/*
 * load_inode: load the inode into RAM.
 * @para ctrl: the global control of hlfs.
 * @para segfile: the segment file name.
 * @para inode_addr: the inode address.
 * @return: if error happens return NULL, if right return inode's RAM address.
 */
struct inode *load_inode(struct back_storage * storage,uint64_t inode_storage_addr)
{
	HLOG_DEBUG("enter func %s",__func__);
	bs_file_t file = NULL;
	struct inode *my_inode = (struct inode*)g_malloc0(sizeof(struct inode));
	if (NULL== my_inode) {
		HLOG_ERROR("Allocate Error!");
		return NULL;
	}

	uint32_t offset = get_offset(inode_storage_addr); 
    const char segfile[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(get_segno(inode_storage_addr),segfile);
	HLOG_DEBUG("inode_addr %lld,offset %u", inode_storage_addr,offset);
	if (0 == storage->bs_file_is_exist(storage, segfile)) {
		if (NULL == (file = storage->bs_file_open(storage, segfile, BS_READONLY))) {
			g_free(my_inode);
			HLOG_ERROR("open segfile error!");
			return NULL;
		}
		if (sizeof(struct inode) != storage->bs_file_pread(storage, file,(char*)my_inode, sizeof(struct inode),offset)) {
			g_free(my_inode);
			storage->bs_file_close(storage, file);
			HLOG_ERROR("pread error!");
			return NULL;
		}
	} else {
		HLOG_ERROR("segfile not exist!");
		g_free(my_inode);
		return NULL;
	}
	
	HLOG_DEBUG("leave func %s", __func__);
	return my_inode;
}


struct inode * load_latest_inode(struct back_storage *storage) 
{
    HLOG_DEBUG("enter func %s",__func__);
    uint32_t segno;
    uint32_t offset;
    int ret = get_cur_latest_segment_info(storage,&segno,&offset);
    if(ret !=0){
       return NULL;
    }
    HLOG_DEBUG("cur latest segno:%d",segno);
    uint64_t inode_storage_addr = get_last_inode_storage_addr_in_seg(storage,segno);
    HLOG_DEBUG("cur latest inode_storage_addr:%llu",inode_storage_addr);
    struct inode * latest_inode = load_inode(storage,inode_storage_addr); 
	HLOG_DEBUG("leave func %s", __func__);
    return latest_inode;
}

/*
 * get_db_addr: get the particular datablock number's address.
 * @para ctrl: the global control structure of hlfs.
 * @para inode: the inode structure.
 * @para db_no: the particular data block number.
 * @return: right return particular db's address, error return -1.
 */
uint64_t get_db_storage_addr_in_inode(struct back_storage * storage,
			struct inode *inode, uint32_t db_no,uint32_t block_size) 
{
    //HLOG_DEBUG("enter func %s",__func__);
	uint64_t cur_storage_addr = 0;
    guint32 BLOCKSIZE = block_size;
    uint32_t IB_ENTRY_NUM = BLOCKSIZE/sizeof(uint64_t);
	if (is_db_in_level1_index_range(db_no)) {
		int idx = db_no % 12;
		cur_storage_addr = inode->blocks[idx];
	} else if (is_db_in_level2_index_range(db_no)) {
		if (0 == inode->iblock) {
			return 1;
		}

		uint64_t *ib = (uint64_t*)alloca(BLOCKSIZE);
	    if( 0!= read_block(storage, inode->iblock, BLOCKSIZE,ib)){
			return -1;
		}
		int idx = (db_no - 12) % IB_ENTRY_NUM;
		cur_storage_addr = *(ib + idx);
		//g_free(ib);
	} else if (is_db_in_level3_index_range(db_no)) {
		if(0 == inode->doubly_iblock) {
			return 1;
		}
		uint64_t *ib = (uint64_t*)alloca(BLOCKSIZE);
		if( 0!= read_block(storage, inode->doubly_iblock, BLOCKSIZE,ib)){
			return -1;
		}
		int idx = (db_no - 12 - IB_ENTRY_NUM) / IB_ENTRY_NUM;
		if (0 == *(ib + idx)) {
			return 1;
		}
		uint64_t *ib2 = (uint64_t*)alloca(BLOCKSIZE);
		if( 0 != read_block(storage, *(ib + idx), BLOCKSIZE,ib2)){
			return -1;
		}
		uint64_t idx2 = (db_no - 12 - IB_ENTRY_NUM) % IB_ENTRY_NUM;
		cur_storage_addr = *(ib2 + idx2);
		//g_free(ib);
		//g_free(ib2);
	} else if (is_db_in_level4_index_range(db_no)) {
		if(0 == inode->triply_iblock) {
			return 1;
		}
		uint64_t *ib = (uint64_t*)alloca(BLOCKSIZE);
		if( 0!=read_block(storage,inode->triply_iblock, BLOCKSIZE,ib)){
			return -1;
		}
		int idx = (db_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM * IB_ENTRY_NUM) / (IB_ENTRY_NUM * IB_ENTRY_NUM);
		if (0 == *(ib + idx)) {
			return 1;
		}
		uint64_t *ib2 =(uint64_t*)alloca(BLOCKSIZE);
		if( 0 != read_block(storage, *(ib + idx), BLOCKSIZE,ib2)){
			return -1;
		}
		uint64_t idx2 = (db_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM * IB_ENTRY_NUM) / IB_ENTRY_NUM % IB_ENTRY_NUM;
		if (0 == *(ib2 + idx2)) {
			return 1;
		}
		uint64_t *ib3 = (uint64_t*)alloca(BLOCKSIZE);
		if( 0!= read_block(storage, *(ib2 + idx2), BLOCKSIZE,ib3)){
			return -1;
		}
		uint64_t idx3 = (db_no - 12 - IB_ENTRY_NUM - IB_ENTRY_NUM * IB_ENTRY_NUM) % IB_ENTRY_NUM;
		cur_storage_addr = *(ib3 + idx3);
		//g_free(ib);
		//g_free(ib2);
		//g_free(ib3);
	} else {
		HLOG_ERROR("index error!");
		return -1;
	}
	//HLOG_DEBUG("leave func %s", __func__);
	return cur_storage_addr;
}
#if 1
uint64_t  SEGMENT_SIZE;
uint64_t  SEGMENT_SIZE_MASK = 0;
uint64_t  SEGMENT_SIZE_SHIFT = 0;
uint32_t  HBLOCK_SIZE;
#endif


static GKeyFile *  get_superblock_keyfile(struct back_storage *storage){
    int ret = 0;
    if(0!=storage->bs_file_is_exist(storage,"superblock")){
        HLOG_ERROR("superblock file can not be accessable");
        ret = -1;
        return ret;
    }
    char key_file_buf[4096];
    bs_file_t file = storage->bs_file_open(storage,"superblock",BS_READONLY);
    if(file == NULL){
      HLOG_ERROR("can not open superblock file");  
      ret = -1;
      goto out;
    }
    int size = storage->bs_file_pread(storage,file,key_file_buf,4096,0);
    if(size <0){
      HLOG_ERROR("can not read superblock file");
      storage->bs_file_close(storage,file);
      goto out;
    }
    GKeyFile * sb_keyfile = g_key_file_new();
    if(FALSE == g_key_file_load_from_data(sb_keyfile,key_file_buf,size,G_KEY_FILE_NONE,NULL)){
       HLOG_ERROR("superblock file format is not key value pairs");
       goto out;
    }
out:
    storage->bs_file_close(storage,file);
    return sb_keyfile;
}

int  read_fs_meta_all(struct back_storage *storage,uint32_t *segment_size,uint32_t *block_size,uint64_t *max_fs_size,
					    gchar **father_uri,uint64_t *snapshot_inode, uint32_t *from_segno){
     GKeyFile * sb_keyfile = get_superblock_keyfile(storage);
     if(NULL == sb_keyfile){
	 	HLOG_ERROR("read superblock keyfile error ");
	 	return -1;
     }	 	
     gchar * _uri =  g_key_file_get_string(sb_keyfile,"METADATA","uri",NULL);
     guint32 _seg_size = g_key_file_get_integer(sb_keyfile,"METADATA","segment_size",NULL);
     guint32 _block_size = g_key_file_get_integer(sb_keyfile,"METADATA","block_size",NULL);
     guint64 _max_fs_size = g_key_file_get_int64(sb_keyfile,"METADATA","max_fs_size",NULL); 
     if(_uri==NULL || _seg_size == 0 || _block_size ==0 || _max_fs_size == 0){
          HLOG_ERROR("superblock parse error");
          g_free(_uri);
          return -1;
     }
    *segment_size = _seg_size;
    *block_size = _block_size;
    *max_fs_size = _max_fs_size;
    if(0!=strcmp(storage->uri,_uri)){
        HLOG_ERROR("error fs name");
        g_free(_uri);
        return -1;
    }
     g_free(_uri);
     gchar * _father_uri =  g_key_file_get_string(sb_keyfile,"METADATA","father_uri",NULL);
     guint64 _snapshot_inode = g_key_file_get_int64(sb_keyfile,"METADATA","snapshot_inode",NULL);
     guint32 _from_segno = g_key_file_get_integer(sb_keyfile,"METADATA","from_segno",NULL);
     if(_father_uri !=NULL){
	   *father_uri = _father_uri;
	   *snapshot_inode = _snapshot_inode;
	   *from_segno = _from_segno;
     }	 
#if 1
    SEGMENT_SIZE = _seg_size;
    SEGMENT_SIZE_MASK  = SEGMENT_SIZE - 1;
    SEGMENT_SIZE_SHIFT = 0;
    while (0 != (SEGMENT_SIZE = (SEGMENT_SIZE >> 1)))
    {                                                                                                         
        SEGMENT_SIZE_SHIFT++;
    }
    HBLOCK_SIZE=_block_size;
#endif	 
     g_key_file_free(sb_keyfile); 	 
     return 0;
}

int read_fs_meta(struct back_storage *storage,uint32_t *segment_size,uint32_t *block_size,uint64_t *max_fs_size)
{
     int ret = 0;
     GKeyFile * sb_keyfile = get_superblock_keyfile(storage);
     gchar * _uri =  g_key_file_get_string(sb_keyfile,"METADATA","uri",NULL);
     guint32 _seg_size = g_key_file_get_integer(sb_keyfile,"METADATA","segment_size",NULL);
     guint32 _block_size = g_key_file_get_integer(sb_keyfile,"METADATA","block_size",NULL);
     guint64 _max_fs_size = g_key_file_get_int64(sb_keyfile,"METADATA","max_fs_size",NULL); 
     if(_uri==NULL || _seg_size == 0 || _block_size ==0 || _max_fs_size == 0){
          HLOG_ERROR("superblock parse error");
          g_free(_uri);
          return -1;
     }
    *segment_size = _seg_size;
    *block_size = _block_size;
    *max_fs_size = _max_fs_size;
#if 1
    SEGMENT_SIZE = _seg_size;
    SEGMENT_SIZE_MASK  = SEGMENT_SIZE - 1;
    SEGMENT_SIZE_SHIFT = 0;
    while (0 != (SEGMENT_SIZE = (SEGMENT_SIZE >> 1)))
    {                                                                                                         
        SEGMENT_SIZE_SHIFT++;
    }
    HBLOCK_SIZE=_block_size;
#endif
out:
    g_key_file_free(sb_keyfile);
    HLOG_DEBUG("leave func %s", __func__);
    return ret;
}
 

int load_latest_inode_map_entry(struct back_storage *storage,
			const uint32_t segno, const uint32_t last_offset,
				struct inode_map_entry *ime)
{
   HLOG_DEBUG("enter func %s", __func__);
   HLOG_DEBUG("777 dbg last offset is %u, seg no is %u", last_offset, segno);
   int ret = 0;
   const char segfile_name[SEGMENT_FILE_NAME_MAX];
   memset((char *) segfile_name, SEGMENT_FILE_NAME_MAX, 0);
   ret = build_segfile_name(segno,segfile_name);
   if (-1 == ret) {
	   HLOG_ERROR("build segfile name error!");
	   return -1;
   }
   bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY); 
   if(file==NULL){
      HLOG_ERROR("can not open segment file %s",segfile_name);
      return -1; 
   }
   HLOG_DEBUG("777 dbg last offset is %d", last_offset);
   uint64_t inode_map_entry_pos = (uint64_t) last_offset - sizeof(struct inode_map_entry);
   HLOG_DEBUG("777 dbg inode map entry pos %llu", inode_map_entry_pos);
   if(sizeof(struct inode_map_entry) != 
                 storage->bs_file_pread(storage,
                 file,(char*)ime,
                 sizeof(struct inode_map_entry),
                 inode_map_entry_pos)){
       HLOG_ERROR("can not read inode map entry ");
       ret = -1;
    }
    storage->bs_file_close(storage,file);
    HLOG_DEBUG("leave func %s", __func__);
    return ret;
}


int file_append_contents(struct back_storage *storage,const char* filename,const char* contents,uint32_t size){
	HLOG_DEBUG("enter func %s", __func__);
    if(storage == NULL || filename == NULL || contents == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }
	int ret = 0;
	bs_file_t file = NULL;
	if ( 0!= storage->bs_file_is_exist(storage,filename)) {
		HLOG_DEBUG("file:%s not exist, create file",filename);
		file = storage->bs_file_create(storage,filename);
		if (NULL == file) {
            ret = -1;
			HLOG_ERROR("can not create file %s",filename);
			goto out;
		}
		storage->bs_file_close(storage,file);
	}
	file = storage->bs_file_open(storage,filename,BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", filename);
        ret = -1;
		goto out;
	}
	if (size !=  storage->bs_file_append(storage, file,contents,size)) {
		HLOG_ERROR("write file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("leave func %s", __func__);
	return ret;

}

int file_get_contents(struct back_storage *storage,const char* filename,char**contents,uint32_t *size){
	HLOG_DEBUG("enter func %s", __func__);
     if(storage == NULL || filename == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }

	int ret = 0;
	int i = 0;
	bs_file_t file = NULL;
	HLOG_DEBUG("filename is %s", filename);
	if ( 0!= storage->bs_file_is_exist(storage,filename)) {
		HLOG_ERROR("file is not exist, but may be first start, not error, check it please");
		return -1;
	}
	bs_file_info_t *file_info = storage->bs_file_info(storage,filename);
	if (NULL == file_info) {
		HLOG_ERROR("get snapshot info error!");
		ret = -1;
		goto out;
	}
	*size = file_info->size; 
	g_free(file_info);
	HLOG_DEBUG("file_size : %u", *size);
	*contents = (char *)g_malloc0(*size);
	if (NULL == *contents) {
		HLOG_ERROR("Allocate error!");
		ret = -1;
		goto out;
	}
	
	file = storage->bs_file_open(storage,filename, BS_READONLY);
	if (file == NULL) {
		HLOG_ERROR("open snapshot.txt error");
		ret = -1;
		goto out;
	}
	if(*size != storage->bs_file_pread(storage,file,*contents,*size,0))
	{
		HLOG_ERROR("Read file:%s failed",filename);
		storage->bs_file_close(storage, file);
              g_free(contents);
		ret = -1;
        goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}







