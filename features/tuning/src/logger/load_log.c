#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "address.h"
#include "storage.h"

int load_log(struct back_storage * storage, guint64 storage_address, struct log_header **log)
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    uint32_t offset = get_offset(storage_address); 
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(get_segno(storage_address),segfile_name);
    struct log_header lh;
    bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY);
    if (NULL == file) {
	    HLOG_ERROR("bs_file_open error!");
       ret = -1;
       goto out;
    } 
    if (LOG_HEADER_LENGTH != storage->bs_file_pread(storage,file, (char*)&lh, LOG_HEADER_LENGTH, offset)) {
	    HLOG_ERROR("bs_file_pread error!");
        ret = -1;
        goto out;
    }
    *log = (struct log_header *)g_malloc0(lh.log_size);
    if (!(*log)) {
	    HLOG_ERROR("allocate error");
	    ret = -1;
		goto out;
    }
    if (lh.log_size != storage->bs_file_pread(storage,file, (char*)*log, lh.log_size, offset)){
	    HLOG_ERROR("bs_file_pread error!");
        ret = -1;
        g_free(*log);
        goto out;
    }
out:
	if(0 != storage->bs_file_close(storage, file)) {
	    HLOG_ERROR("bs_file_close error!");
		ret = -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}


