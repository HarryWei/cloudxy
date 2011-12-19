/*
 *  hlfs_open.c
 *  Kanghua <kanghua151@msn.com> (C) 2011
 *  Updated by Harry Wei <harryxiyou@gmail.com>
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <glib.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "logger.h"

/*
 * load_latest_inode: load the lastest inode structure for ctrl.
 * @param ctrl: the global control for our FS.
 * @return: if successful return 0, else return -1.
 */
static int load_latest_inode(struct hlfs_ctrl *ctrl)
{
	HLOG_DEBUG("enter func %s", __func__);
    if (NULL == ctrl) {
        HLOG_ERROR("input parameter error");
        return -1;
    }
    int ret = 0; 
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(ctrl->last_segno,segfile_name);
    bs_file_t file = ctrl->storage->bs_file_open(ctrl->storage,segfile_name,BS_READONLY); 
    if(file==NULL){
        HLOG_ERROR("can not open segment file %s",segfile_name);
        goto out2; 
    }
    uint64_t inode_pos = ctrl->last_offset - 
        sizeof(struct inode_map_entry) -
        sizeof(struct inode);
    HLOG_DEBUG("inode pos  %llu",inode_pos);
    if(sizeof(struct inode) != ctrl->storage->bs_file_pread(ctrl->storage,
				file,(char*)&ctrl->inode, sizeof(struct inode), inode_pos)){
       HLOG_ERROR("can not read inode from %s",segfile_name);
       ret = -1;
       goto out1;
    }
out1:
    ctrl->storage->bs_file_close(ctrl->storage,file);
out2:
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}

/*
 * hlfs_open: open a file.
 * @param ctrl: the global control.
 * @param flag: the flag for open operation, flag == 0
 *        readonly and flag == 1 writable.
 * @return: if successful return 0, else return -1.
 */
int hlfs_open(struct hlfs_ctrl *ctrl, int flag)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (ctrl==NULL ||(flag != 0 && flag != 1)) { /* check the parameters */
		HLOG_ERROR("error params :falg %d",flag);
		return -1;
	}
	if(ctrl->usage_ref > 0){
		HLOG_DEBUG("This fs has opened by other,can not use it"); 
	}
    int ret;
    HLOG_DEBUG("inode no %llu , inode address %llu", ctrl->imap_entry.inode_no, ctrl->imap_entry.inode_addr);
    if (ctrl->imap_entry.inode_no == 0 && 
			ctrl->imap_entry.inode_addr == 0) { /* no inode condition */
		HLOG_DEBUG("empty filesystem %s", ctrl->sb.fsname);
        if (flag == 0) {
			HLOG_ERROR("must create it with writeable flag");
			return -1;
		}
        HLOG_DEBUG("create new fs inode !");
        ctrl->inode.length = 0;
        ctrl->inode.mtime = get_current_time();
        ctrl->inode.ctime = get_current_time();
        ctrl->inode.atime = get_current_time();
	} else { /* exist inode */
		HLOG_DEBUG("open exist fs %s", ctrl->sb.fsname);
		if (0 != load_latest_inode(ctrl)) { /* get the lastest inode structure */ 
			HLOG_ERROR("fail to load inode !");
            return -1; 
		}
		HLOG_DEBUG("inode 's length:%llu",ctrl->inode.length);
	}
#if 0
	g_message("append log with only inode !\n");
    int size = append_inode(ctrl); /* append new log */
    if (size < 0) {
		g_message("fail to append log with inode ! %d\n",size);
        return -1;
	}
    ctrl->last_offset += size;
#endif
	ctrl->usage_ref++;
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}


