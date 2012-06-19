
/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */#ifndef _HLFS_MISC_H_
#define _HLFS_MISC_H_

#include <stdio.h>
#include <stdint.h>
#include "storage.h"

#ifdef __cplusplus  
extern "C" {
#endif
uint64_t get_current_time(void);
int build_segfile_name(uint32_t segno, const char* segfile);
int build_segfile_name_by_address(uint64_t storage_address, const char* segfile);
uint32_t get_segfile_no(const char * segfile);
int read_block(struct back_storage *storage ,uint64_t storage_address,uint32_t block_size,char * block_buf);
int parse_from_uri(const char *uri,char ** head, char** hostname ,char** dir,char** fs_name,int* port);

#ifdef __cplusplus 
} 
#endif 
#endif 
