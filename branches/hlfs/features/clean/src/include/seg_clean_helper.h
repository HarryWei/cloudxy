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
int seg_usage2text(struct segment_usage* segment_usage,char *textbuf);
int dump_seg_usage_text(struct back_storage * storage,const char*segment_usage_file, const char * seg_usage_text);
int load_seg_usage_text(struct back_storage *storage,SEG_USAGE_T *seg_usage, const char *textbuf);
int get_refer_inode(struct back_storage *storage,uint64_t segno,GHashTable* seg_usage_hashtable);



//int dump_segment_delmark(struct back_storage* storage,const char* segment_delmark_file,uint32_t segno);

#ifdef __cplusplus 
} 
#endif 

#endif

