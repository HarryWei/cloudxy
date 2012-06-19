/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <glib.h>
#include "logger.h"
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage.h"
#include "seg_clean.h"


int dump_seg_usage(struct back_storage * storage,
    const char*segment_usage_file,
    struct segment_usage * seg_usage){
    //HLOG_DEBUG("enter func %s",__func__);
    //HLOG_DEBUG("enter func %s,seg usage file:%s",__func__,segment_usage_file);
    char segtextbuf[4096];
    memset(segtextbuf,0,0);
    uint32_t len = seg_usage2text(seg_usage,segtextbuf);
    int ret = file_append_contents(storage,segment_usage_file,segtextbuf,len);
    //HLOG_DEBUG("leave func %s",__func__);
    return ret;
}
