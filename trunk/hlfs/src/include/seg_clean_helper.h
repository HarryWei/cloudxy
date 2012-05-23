/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_SEG_CLEAN_HELPER_H_
#define _HLFS_SEG_CLEAN_HELPER_H_

#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"
#include "seg_clean.h"



#ifdef __cplusplus  
extern "C" {
#endif

//int segment_delmark2text(uint32_t segno,char *textbuf);
int seg_usage2text(SEG_USAGE_T* seg_usage,char *textbuf);
int seg_usage4text(SEG_USAGE_T * seg_usage, const char *textbuf);

//int dump_seg_usage_text(struct back_storage * storage,const char*segment_usage_file, const char * seg_usage_text);
//int load_seg_usage_text(struct back_storage *storage,SEG_USAGE_T *seg_usage, const char *textbuf);
int load_all_seg_usage  (struct back_storage *storage, const char *seg_usage_file,  GHashTable* seg_usage_hashtable);
int sort_all_seg_usage(GHashTable *ss_hashtable,GList *seg_usage_list);
int get_refer_inode_between_snapshots(struct back_storage *storage,uint32_t segno,GList *snapshot_sorted_list, struct inode ** inode,char** up_sname);

#ifdef __cplusplus 
} 
#endif 

#endif

