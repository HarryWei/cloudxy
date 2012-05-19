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
	//HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    struct snapshot *ss = NULL;
    bs_file_t file = NULL;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("storage init error!");
        return -1;
    }
	if (0!=(ret=load_snapshot_by_name(storage,SNAPSHOT_FILE,&ss,ssname))){
		HLOG_ERROR("snapshot %s is not exist, right???", ssname);
		ret = EHLFS_SSNOTEXIST;
		goto out;
	}
	//HLOG_DEBUG("99 before dump ret is %d", ret);
    if (0!= (ret=dump_snapshot_delmark(storage,SNAPSHOT_FILE,ss->sname))){
        goto out;
    }
	//HLOG_DEBUG("99 after dump ret is %d", ret);

	HLOG_INFO("Remove Snapshot Succ - snapshot_name:%s,last_segno:%d,last_offset:%d",
    						      ssname,
    						      ctrl->last_segno,
    						      ctrl->last_offset);
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
    if(ss!=NULL){
       g_free(ss); 
    }
    //HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
