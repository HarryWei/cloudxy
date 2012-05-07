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
int load_block_by_addr(struct hlfs_ctrl *ctrl,uint64_t pos,char** block);
int load_block_by_no(struct hlfs_ctrl *ctrl,uint64_t no,char** block);
int load_log(void *storage_handler, uint64_t storage_address,struct log_header **log);
int append_log(struct hlfs_ctrl *ctrl,const char *db_buff,uint32_t db_start,uint32_t db_end);
//int append_inode(struct hlfs_ctrl * ctrl);
#ifdef __cplusplus 
} 
#endif 
#endif

