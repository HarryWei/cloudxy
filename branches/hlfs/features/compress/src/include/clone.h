/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef __CLONE_H__
#define __CLONE_H__

//#include "api/hlfs.h"
#include "glib.h"
#include "hlfs_log.h"
#include "comm_define.h"

typedef  struct family_ctrl{
           char*      father_uri;
	    uint64_t  base_father_inode;
	    uint32_t  from_segno;
        GList*    seg_storage_list;
 }FAMILY_CTRL;

FAMILY_CTRL * family_new(void);
int faimly_init(FAMILY_CTRL *family_ctrl,char* father_uri,uint64_t base_inode,uint32_t from_segno);
struct back_storage * get_parent_storage(FAMILY_CTRL *fctrl,uint32_t segno);
int  famliy_destroy(FAMILY_CTRL *fctrl);


#endif
