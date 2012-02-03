#ifndef _HLFS_SEGMENT_CLEANER_H_
#define _HLFS_SEGMENT_CLEANER_H_

#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"

#define ALIVE_LOG_BITMAP 512
#define SEGMENTS_USAGE_FILE "segments_usage.txt"
#define SEGMENTS_DEL_FILE   "segments_delmark.txt"

/* segment usage structure for cleaning task */
struct segment_usage {
	uint64_t segno;				
	uint64_t alive_blocks;
	uint64_t timestamp;
    uint64_t log_num;
	char *bitmap;
};

#ifdef __cplusplus  
extern "C" {
#endif


int segment_delmark2text(uint32_t segno,char *textbuf);
struct segment_usage *get_segment_usage(uint32_t segno);
int segment_usage2text(const struct segment_usage* segment_usage,char *textbuf);
int read_segment_usage(struct back_storage * storage,struct segment_usage* seg_usage,uint32_t segno);
int dump_segment_delmark(struct back_storage* storage,const char* segment_delmark_file,uint32_t segno);
int dump_segment_usage_text(struct back_storage * storage,const char*segment_usage_file, const char * seg_usage_text);
int dump_segment_usage(struct back_storage * storage,const char*segment_usage_file,struct segment_usage * seg_usage);
int load_segment_usage_from_text(struct back_storage *storage,struct segment_usage * seg_usage, const char *textbuf);
int load_all_segment_usage(struct back_storage *storage,const char *seg_usage_file,const char * seg_delmark_file,GHashTable* seg_usage_hashtable);
int segment_usage_calc(struct back_storage* storage, const char *segfile,struct inode * latest_inode ,struct segment_usage *seg_usage,uint32_t block_size);

#ifdef __cplusplus 
} 
#endif 

#endif

