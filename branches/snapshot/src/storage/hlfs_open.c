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
#include "snapshot.h"

static int 
find_ss_name(struct hlfs_ctrl *ctrl, uint64_t inode_addr, char **ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
//	inode_cup_t *inode_cup = (inode_cup_t *)g_malloc0(sizeof(inode_cup_t));
//	inode_cup->cur_inode_addr = inode_addr;
//	inode_cup->up_inode_addr = 0;
#if 0
	if (EHLFS_NOFILE == ctrl->storage->bs_file_is_exist(ctrl->storage, SNAPSHOT_FILE)) {
		*ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
		if (NULL == *ss_name) {
			HLOG_ERROR("Allocate Error!");
			return EHLFS_MEM;
		}
		HLOG_DEBUG("We can not find the inode_addr's snapshot, so use inode addr as up ss name");
		if (0 > create_auto_snapshot(ctrl, inode_addr)) {
			HLOG_ERROR("create auto snapshot error!");
			g_free(*ss_name);
			return EHLFS_FUNC;
		}
		sprintf(*ss_name, "%llu", inode_addr);
		return ret;
	}
#endif
	GHashTable *ss_hashtable = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, NULL);
	if (load_all_ss(ctrl->storage, ss_hashtable) < 0) {
		HLOG_ERROR("load all ss error!");
		g_hash_table_destroy(ss_hashtable);
		HLOG_DEBUG("leave func %s", __func__);
		return -1;
	}
	GList *list = g_hash_table_get_values(ss_hashtable);
	if (list == NULL) {
		HLOG_ERROR("get values error");
		g_hash_table_destroy(ss_hashtable);
		HLOG_DEBUG("leave func %s", __func__);
		return -1;
	}
	int i = 0;
	int len = g_list_length(list);
	*ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
	if (NULL == *ss_name) {
		HLOG_ERROR("Allocate Error!");
		return -1;
	}
	if (0 == len) {
		HLOG_DEBUG("There is no snapshot yet, create adam snapshot ...");
		create_adam_snapshot(ctrl, "adam"); // This is the Adam of the snapshot
		sprintf(*ss_name, "%s", "adam");
		goto out;
	}
	struct snapshot tmp_ss; // Save the lastest snapshot
	uint64_t ss_time = 0;
	memset(&tmp_ss, 0, sizeof(struct snapshot));
	for (i = 0; i < g_list_length(list); i++) {
		struct snapshot *ss = (struct snapshot *) g_list_nth_data(list, i);
		if (NULL == ss) {
			HLOG_ERROR("find the ss error!");
			g_free(*ss_name);
			ret = -1;
			goto out;
		}
		if (ss->timestamp > ss_time) {
			memcpy(&tmp_ss, ss, sizeof(struct snapshot));
			ss_time = ss->timestamp;
		}
		if (inode_addr == ss->inode_addr) {
			sprintf(*ss_name, "%s", ss->sname);
			goto out;
		}
	}
	// NOTE: This is not the up_ss_name in snapshot structure
	HLOG_DEBUG("We can not find the inode_addr's snapshot, so find up snapshot name ...");
	sprintf(*ss_name, "%s", tmp_ss.sname); // Copy the up snapshot name to ctrl->alive_ss_name
#if 0
	if (0 > create_auto_snapshot(ctrl, inode_addr)) {
		HLOG_ERROR("create auto snapshot error!");
		g_free(*ss_name);
		ret = -1;
		goto out;
	}
	sprintf(*ss_name, "%llu", inode_addr);
#endif
out:
	g_list_free(list);
	g_hash_table_destroy(ss_hashtable);
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

#if 1
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
#endif

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
    int ret = 0;
    HLOG_DEBUG("inode no %llu , inode address %llu", ctrl->imap_entry.inode_no, ctrl->imap_entry.inode_addr);
	if (NULL != ctrl->alive_ss_name) {
		g_free(ctrl->alive_ss_name);
		ctrl->alive_ss_name = NULL;
	}
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
		create_adam_snapshot(ctrl, "adam"); // This is the Adam of the snapshot
		ctrl->alive_ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
		if (NULL == ctrl->alive_ss_name) {
			HLOG_ERROR("Allocate error!");
			return EHLFS_MEM;
		}
		sprintf(ctrl->alive_ss_name, "%s", "adam");
	} else { /* exist inode */
		HLOG_DEBUG("open exist fs %s", ctrl->sb.fsname);
		if (0 != load_latest_inode(ctrl)) { /* get the lastest inode structure */ 
			HLOG_ERROR("fail to load inode !");
            return -1; 
		}
		HLOG_DEBUG("inode 's length:%llu",ctrl->inode.length);
		if (EHLFS_NOFILE == (ret = ctrl->storage->bs_file_is_exist(ctrl->storage, SNAPSHOT_FILE))) {
			HLOG_DEBUG("We have no snapshot file, we should re-build it ....");
			create_adam_snapshot(ctrl, "adam"); // This is the Adam of the snapshot
			ctrl->alive_ss_name = (char *)g_malloc0(sizeof(char) * MAX_FILE_NAME_LEN);
			if (NULL == ctrl->alive_ss_name) {
				HLOG_ERROR("Allocate error!");
				return EHLFS_MEM;
			}
			sprintf(ctrl->alive_ss_name, "%s", "adam");
		} else if (EHLFS_MEM == ret) {
			HLOG_DEBUG("Invoke func error");
			return EHLFS_FUNC;
		} else { // find the ssname or the "up-ss-name" or create adam snapshot
			if (0 > find_ss_name(ctrl, ctrl->imap_entry.inode_addr, &ctrl->alive_ss_name)) {
				HLOG_ERROR("find snapshot of inode addr error!");
				return EHLFS_FUNC;
			}
		}
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
	if (ctrl->write_task_run == 0) {
		ctrl->write_task_run = 1;
	}
#if 0
	if (0 > find_ss_name_of_inode(ctrl, ctrl->imap_entry.inode_addr, &ctrl->alive_ss_name)) {
		HLOG_ERROR("find snapshot of inode addr error!");
		return -1;
	}
#endif
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}
