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

SEG_USAGE_T* load_seg_usage_by_order (struct back_storage *storage, const char *seg_usage_file,int *num_entries){
        int ret = 0;
		GHashTable   * seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);
		ret = load_all_seg_usage(storage,seg_usage_file,seg_usage_hashtable);
		GList* seg_list;
    	ret = sort_all_seg_usage(seg_usage_hashtable,&seg_list);
    	g_assert(ret == 0);
    	int size =  g_list_length(seg_list);
		SEG_USAGE_T * seg_usages_buf = (SEG_USAGE_T *)g_malloc0(sizeof(SEG_USAGE_T)*size);
		int i;
        for(i= 0;i<size; i++){
		   SEG_USAGE_T * _seg_usage = (SEG_USAGE_T *)g_list_nth_data(seg_list,i);
		   memcpy(seg_usages_buf+i*sizeof(SEG_USAGE_T),_seg_usage,sizeof(SEG_USAGE_T));
		   g_free(_seg_usage);
        }
		g_list_free(seg_list);
		g_hash_table_destroy(seg_usage_hashtable);
		*num_entries = size;
		return seg_usages_buf;
}


