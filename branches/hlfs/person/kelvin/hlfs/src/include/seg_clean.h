/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_SEG_CLEAN_H_
#define _HLFS_SEG_CLEAN_H_
#define ALIVE_LOG_BITMAP 512
#define SEGMENTS_USAGE_FILE "segments_usage.txt"
//#define SEGMENTS_DEL_FILE   "segments_delmark.txt"
#include "hlfs_ctrl.h"
#include "storage.h"

#define DEF_IO_NONACTIVE_PERIOD  10U
#define DEF_SEG_COPY_WATERLEVEL 128U
#define DEF_SC_CHECK_PERIOD 1U

/* segment usage structure for cleaning task */
typedef struct segment_usage {
	uint32_t segno;	
	char up_sname[HLFS_FILE_NAME_MAX];
	uint64_t inode_addr; /* up snapshot's inode address between snapshot region;if in non-shapshot region,it is pre seg last inode address */
	uint64_t timestamp;   /* create time */
	uint32_t log_num;
	uint32_t block_num;
	uint32_t alive_block_num;
	//uint32_t blitmap_size; 
	char *bitmap; 
}SEG_USAGE_T;

 typedef enum _seg_in_snapshot    
 	{
 	  IN_SNAPSHOT  = 0, 
 	  ABOVE_SNAPSHOT = 1,  
 	  ON_SNAPSHOT = 2,
 	 }SEG_SNAPSHOT;

 static const char* EMPTY_UP_SNAPSHOT="_____";

#ifdef __cplusplus  
extern "C" {
#endif


//int seg_range_usage_statis(struct back_storage *storage,uint32_t segment_size, uint32_t block_size,uint64_t start_segno,uint64_t end_segno,GHashTable *seg_usage_hashtable);
int seg_usage_calc(struct back_storage* storage,uint32_t block_size,uint32_t segno,struct inode *refer_inode,SEG_USAGE_T *seg_usage);
//int load_seg_usage(struct back_storage * storage,uint32_t segno,const char* segment_usage_file,SEG_USAGE_T * seg_usage);
int dump_seg_usage(struct back_storage * storage,  const char* seg_usage_file,  SEG_USAGE_T * seg_usage);
int migrate_alive_blocks (struct hlfs_ctrl *hctrl,SEG_USAGE_T *seg_usage);
SEG_USAGE_T* load_seg_usage_by_order (struct back_storage *storage, const char *seg_usage_file,int *num_entries);


#ifdef __cplusplus 
} 
#endif 

#endif


