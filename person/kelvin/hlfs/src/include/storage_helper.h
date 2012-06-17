/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_STORAGE_HELPER_H_
#define _HLFS_STORAGE_HELPER_H_

#include <stdio.h>
#include <stdint.h>
#include "hlfs_ctrl.h"
#include "storage.h"

#ifdef __cplusplus  
extern "C" {
#endif
int deinit_storage_handler(struct back_storage * storage);
struct inode * load_latest_inode(struct back_storage *storage);
//struct back_storage* init_storage_handler(const char* uri,const char *fs_name);
struct back_storage* init_storage_handler(const char* uri);
struct inode *load_inode(struct back_storage * storage,uint64_t inode_storage_addr);
int  read_fs_meta(struct back_storage *storage,uint32_t *segment_size,uint32_t *block_size,uint64_t *max_fs_size);
int  read_fs_meta_all(struct back_storage *storage,uint32_t *segment_size,uint32_t *block_size,uint64_t *max_fs_size,
					    gchar **father_uri,uint64_t *base_father_inode, uint32_t *from_segno);
uint64_t get_last_inode_storage_addr_in_seg( struct back_storage * storage, uint32_t segno);
int get_cur_latest_segment_info(struct back_storage * storage,uint32_t *segno,uint32_t *offset);
uint64_t get_db_storage_addr_in_inode(struct back_storage * storage,struct inode *inode, uint32_t db_no,uint32_t block_size);
int load_latest_inode_map_entry(struct back_storage *storage,uint32_t segno,uint32_t last_offset,struct inode_map_entry *ime);
int file_get_contents(struct back_storage *storage,const char* filename,char**constents,uint32_t *size);
int file_append_contents(struct back_storage *storage,const char* filename,const char* contents,uint32_t size);
#ifdef __cplusplus 
} 
#endif 

#endif
