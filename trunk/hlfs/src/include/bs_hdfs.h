/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_BS_HDFS_H_
#define _HLFS_BS_HDFS_H_

#include <stdint.h>
#include "glib.h"
#include "storage.h"

#ifdef __cplusplus  
extern "C" {
#endif

int hdfs_connect(struct back_storage *storage,const char* uri);
int hdfs_disconnect(struct back_storage * storage);
bs_file_t hdfs_file_open(struct back_storage *storage,const char *path,int flags);
int hdfs_file_close(struct back_storage *storage,bs_file_t file);
int hdfs_file_is_exist(struct back_storage *storage,const char *path);
uint64_t hdfs_file_tell(struct back_storage *storage,bs_file_t file);
int hdfs_file_append(struct back_storage *storage,bs_file_t file,const char *write_buff,uint32_t write_len);
int hdfs_file_pread(struct back_storage *storage,bs_file_t file,const char*read_buff,uint32_t read_len,uint64_t pos);
int hdfs_file_flush(struct back_storage *storage,bs_file_t file);
int hdfs_file_delete(struct back_storage *storage,const char* path);
bs_file_info_t * hdfs_file_info(struct back_storage *storage,const char* path);
bs_file_info_t* hdfs_list_dir(struct back_storage *storage,const char *dir_path,uint32_t* num_entries);
int hdfs_file_mkdir(struct back_storage *storage,const char *path);
bs_file_t hdfs_file_create(struct back_storage *storage,const char *path);

#ifdef __cplusplus 
} 
#endif 
#if 0
static struct back_storage hdfs_storage = {
   .storage_name = "hdfs",
   .fs_location=NULL,
   .fs_name=NULL,
   .fs_handler=NULL,
   .user=NULL,
   .port=0,
   bs_fs_connect:hdfs_connect,
   bs_fs_disconnect:hdfs_disconnect,
   bs_file_open:hdfs_file_open,
   bs_file_close:hdfs_file_close,
   bs_file_is_exist:hdfs_file_is_exist,
   bs_file_tell:hdfs_file_tell,
   bs_file_append:hdfs_file_append,
   bs_file_pread:hdfs_file_pread,
   bs_file_flush:hdfs_file_flush,
   bs_file_delete:hdfs_file_delete,
   bs_file_create:hdfs_file_create,
   bs_file_mkdir:hdfs_file_mkdir,
   bs_file_info:hdfs_file_info,
   bs_file_list_dir:hdfs_list_dir,
};

#else
static struct back_storage * get_hdfs_storage_ops(){
       struct back_storage * storage = (struct back_storage*)malloc(sizeof(struct back_storage));
       storage->storage_name = "hdfs";
       storage->uri=NULL;
//       storage->fs_name=NULL;
       storage->fs_handler=NULL;
       storage->user=NULL;
       storage->port=0;
       storage->bs_fs_connect = hdfs_connect;
       storage->bs_fs_disconnect = hdfs_disconnect,
       storage->bs_file_open = hdfs_file_open;
       storage->bs_file_close = hdfs_file_close;
       storage->bs_file_is_exist = hdfs_file_is_exist;
       storage->bs_file_tell = hdfs_file_tell;
       storage->bs_file_append = hdfs_file_append;
       storage->bs_file_pread = hdfs_file_pread;
       storage->bs_file_flush = hdfs_file_flush;
       storage->bs_file_delete = hdfs_file_delete;
       storage->bs_file_create = hdfs_file_create;
       storage->bs_file_mkdir = hdfs_file_mkdir;
       storage->bs_file_info = hdfs_file_info;
       storage->bs_file_list_dir = hdfs_list_dir;
       return storage;
}

#endif 

#endif 
