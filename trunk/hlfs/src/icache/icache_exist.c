/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "hlfs_log.h"
#include "glib.h"
#include "icache.h"
gboolean  icache_iblock_exist(ICACHE_CTRL *icache_ctrl, uint32_t block_no){
    //HLOG_DEBUG("--Entering func %s", __func__);
    iblock_t * iblock = icache_query(icache_ctrl,block_no);
    if(iblock==NULL){
       return FALSE;
    }
    return TRUE;
}
