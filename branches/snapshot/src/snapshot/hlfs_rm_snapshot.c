/*
 *  src/snapshot/hlfs_rm_snapshot.c
 
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

int hlfs_rm_snapshot(const char *uri,const char *ssname) {
    HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    bs_file_t file = NULL;
    struct back_storage *storage = init_storage_handler(uri);
    if (NULL == storage) {
        HLOG_ERROR("storage init error!");
        return -1;
    }
    if (-1 == storage->bs_file_is_exist(storage, SNAPSHOT_FILE)) {
        HLOG_DEBUG("snapshot del file not exist, create it");
        file = storage->bs_file_create(storage, SNAPSHOT_FILE);
        if (NULL == file) {
			HLOG_ERROR("can not create snapshot del file!");
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage, SNAPSHOT_FILE, BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open snapshot del file");
		goto out;
	}
    char deltext[128];
	memset(deltext, 0, 128);
	uint32_t len = snapshot_delmark2text(ssname, deltext);
	HLOG_DEBUG("delbuf is %s", deltext);
	if (0 > storage->bs_file_append(storage, file, deltext, len)) {
		HLOG_ERROR("append del text failed!");
		ret = -1;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
    HLOG_DEBUG("leave func %s", __func__);
	return ret;
}
