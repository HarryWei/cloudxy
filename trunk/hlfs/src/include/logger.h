/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_LOGGER_H_
#define _HLFS_LOGGER_H_

#include <time.h>
#include <stdint.h>
#include "glib.h"
#include "hlfs_ctrl.h"

#ifdef __cplusplus  
extern "C" {
#endif
int seg_clean_task(struct hlfs_ctrl *ctrl);
int load_block_by_addr(struct hlfs_ctrl *ctrl,uint64_t pos,char* block);
int load_block_by_no(struct hlfs_ctrl *ctrl,uint32_t no,char* block);
int load_block_by_addr_fast(struct hlfs_ctrl *ctrl,uint64_t pos,char* block);
int load_block_by_no_fast(struct hlfs_ctrl *ctrl,uint32_t no,char* block);

int read_layer1_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);	
int read_layer2_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);
int read_layer3_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);
int write_layer1_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);
int write_layer2_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);
int write_layer3_iblock(struct hlfs_ctrl *hctrl,uint32_t dbno,char *iblock);

int append_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end);
int prev_open_rsegfile(struct hlfs_ctrl *ctrl,uint32_t segno);
int prev_open_wsegfile(struct hlfs_ctrl *ctrl);
int read_block_fast(struct hlfs_ctrl *ctrl,uint64_t storage_address,char* block);

//int append_inode(struct hlfs_ctrl * ctrl);
#ifdef __cplusplus 
} 
#endif 
#endif

