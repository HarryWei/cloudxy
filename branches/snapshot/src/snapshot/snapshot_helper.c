#include <stdint.h>
#include <hlfs_ctrl.h>
#include <stdio.h>
#include "hlfs_log.h"
#include "snapshot.h"

int snapshot2text(const struct snapshot* snapshot, char*textbuf){
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	memset(textbuf, 0, sizeof(struct snapshot) * 2);
	int n = sprintf(textbuf, "%s %llu %llu %s\n","+",snapshot->timestamp, snapshot->inode_addr, snapshot->sname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}
int dump_snapshot(struct back_storage *storage,const char* snapshot_file,struct snapshot * snapshot){
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
    if(snapshot_file == NULL || snapshot == NULL || storage == NULL){
        return -1;
    }
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;
	if (-1 == storage->bs_file_is_exist(storage,snapshot_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = storage->bs_file_create(storage,snapshot_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s",snapshot_file);
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage,snapshot_file,BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", snapshot_file);
		goto out;
	}
    char snapshot_text[1024];
    memset(snapshot_text,0,1024);
	len = snapshot2text(snapshot,snapshot_text);
	HLOG_DEBUG("cp text is %s", snapshot_text);
	if (len !=  storage->bs_file_append(storage, file,snapshot_text, len)) {
		HLOG_ERROR("write cp file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}
int snapshot_delmark2text(const char*ssname, char*textbuf){
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
	memset(textbuf, 0, sizeof(struct snapshot) * 2);
	int n = sprintf(textbuf, "%s %s\n","-",ssname);
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return n;
}

int dump_snapshot_delmark(struct back_storage *storage,const char* snapshot_file,const *ssname){
	HLOG_DEBUG("dbg 77 enter func %s", __func__);
    if(snapshot_file == NULL || ssname == NULL || storage == NULL){
        return -1;
    }
	int ret = 0;
	int len = 0;
	bs_file_t file = NULL;
	if (-1 == storage->bs_file_is_exist(storage,snapshot_file)) {
		HLOG_DEBUG("cp file not exist, create cp file");
		file = storage->bs_file_create(storage,snapshot_file);
		if (NULL == file) {
			HLOG_ERROR("can not create cp file %s",snapshot_file);
			goto out;
		}
		storage->bs_file_close(storage, file);
	}
	file = storage->bs_file_open(storage,snapshot_file,BS_WRITEABLE);
	if (NULL == file) {
		HLOG_ERROR("can not open cp file %s", snapshot_file);
		goto out;
	}
    char snapshot_delmark_text[1024];
    memset(snapshot_delmark_text,0,1024);
	len = snapshot_delmark2text(ssname,snapshot_delmark_text);
	HLOG_DEBUG("cp text is %s", snapshot_delmark_text);
	if (len !=  storage->bs_file_append(storage, file,snapshot_delmark_text, len)) {
		HLOG_ERROR("write cp file error, write bytes %d", ret);
		ret = -1;
		goto out;
	}
out:
	if (NULL != file) {
		storage->bs_file_close(storage, file);
	}
	HLOG_DEBUG("dbg 77 leave func %s", __func__);
	return ret;
}


