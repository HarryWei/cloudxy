/*
 *  src/snapshot/hlfs_rm_snapshot.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "snapshot.h"
#include "storage_helper.h"
#include "hlfs_log.h"

int 
hlfs_rm_snapshot(const char *uri,const char *ssname) {
    int ret = 0;
    struct snapshot *ss = NULL;
    bs_file_t file = NULL;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("storage init error!");
        return -1;
    }
	if (0!=(ret=load_snapshot_by_name(storage,SNAPSHOT_FILE,ss,ssname))){
		HLOG_DEBUG("snapshot %s is not exist, right???", ssname);
		goto out;
	}
    if (0!= (ret=dump_snapshot_delmark(storage,SNAPSHOT_FILE,ss))){
        goto out;
    }
    
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
    if(ss!=NULL){
       g_free(ss); 
    }
    HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
