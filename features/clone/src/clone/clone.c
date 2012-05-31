/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"
#include "storage.h"
#include "storage_helper.h"
#include "bs_local.h"
#include "seg_clean.h"
#include "logger.h"
#include "clone.h"


FAMILY_CTRL * family_new(){
    FAMILY_CTRL * family = (FAMILY_CTRL*)g_malloc0(sizeof(FAMILY_CTRL));
    return family;
}


typedef struct storage_item{
	struct back_storage* storage;
	uint32_t segno;
}STORAGE_ITEM;

int faimly_init(FAMILY_CTRL *fctrl,char* furi,uint64_t fbase_inode,uint32_t fsegno){
	int ret = 0;
	
	struct back_storage *storage =NULL;
	char *uri = furi;
	char * father_uri = NULL;
    uint64_t base_father_inode,max_fs_size;
    uint32_t from_segno,seg_size,block_size;
	STORAGE_ITEM *storage_item = NULL;
    from_segno = fsegno;
    do{
	   father_uri=NULL;
	   if(NULL == (storage = init_storage_handler(uri))){
	   	  HLOG_ERROR("fail to init storage for uri:%s",uri);
		  ret = -1;
	  	  goto out;;
	   }	
	   //g_tree_insert(fctrl->seg_storage_map,GINT_TO_POINTER((uint32_t)(segno-1),storage);
	   storage_item = g_malloc0(sizeof(STORAGE_ITEM));
	   storage_item->storage = storage;
	   storage_item->segno = from_segno -1;
	   HLOG_DEBUG("from_segno:%d\n",from_segno);
	   fctrl->seg_storage_list = g_list_append(fctrl->seg_storage_list,storage_item);
       if(0 !=(ret = read_fs_meta_all(storage,&seg_size,&block_size,&max_fs_size,
		   			            &father_uri,&base_father_inode,&from_segno))){
		  HLOG_ERROR("fail to read fs meta info");
		  ret = -1;
		  goto out;
       }
       if(father_uri!=NULL){
	   	  HLOG_DEBUG("need read uri:%s 's father uri:%s",uri,father_uri);
		  HLOG_DEBUG("from_segno:%d",from_segno);
		  uri = father_uri;
       }else{
          HLOG_DEBUG("need read uri:%s is first uri",uri);
		  break;
       }
	}while(1);
    fctrl->from_segno = fsegno;
    fctrl->father_uri = g_strdup(furi);
    fctrl->base_father_inode = fbase_inode;
out:
	return ret;
}

struct back_storage * get_parent_storage(FAMILY_CTRL *fctrl, uint32_t segno){
        struct  back_storage * storage = NULL;
	 int i = 0;
	 for(i = g_list_length(fctrl->seg_storage_list)-1; i >= 0; i--){
        STORAGE_ITEM *storage_item = g_list_nth_data(fctrl->seg_storage_list,i);
        HLOG_DEBUG("storage_item's segno:%d,search segno:%d",storage_item->segno,segno);
        if(storage_item->segno >= segno){
		   storage = storage_item->storage;
           break; 
        }
    }
    return storage;
}

int family_destroy(FAMILY_CTRL *fctrl){
    if(fctrl == NULL){
	  return 0;
    }

    int i = 0;
    for(i = g_list_length(fctrl->seg_storage_list)-1; i >= 0; i--){
        STORAGE_ITEM *storage_item = g_list_nth_data(fctrl->seg_storage_list,i);
	 deinit_storage_handler(storage_item->storage);
        g_free(storage_item);
    }
    g_list_free(fctrl->seg_storage_list);
    g_free(fctrl->father_uri);
    g_free(fctrl);
    return 0;
}
