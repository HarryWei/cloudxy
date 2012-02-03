#include "api/hlfs.h"
#include "snapshot.h"
#include "misc.h"
#include "storage_helper.h"
#include "string.h"
#include "hlfs_log.h"
#include "logger.h"

#define MAX_BUFSIZE (1 * 1024)


int take_snapshot(HLFS_CTRL *ctrl, const char *ss_name)
{
	if (ctrl->imap_entry.inode_addr == 0) {
		HLOG_DEBUG("No data in HLFS.");
		return 1;
	}
	HLOG_DEBUG("enter func %s", __func__);
	g_mutex_lock(ctrl->hlfs_access_mutex);
	int ret = 0;
	struct snapshot *ss;
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));

	sprintf(ss->ss_name, "%s", ss_name);
	sprintf(ss->up_ss_name, "null");
#if 0
	g_message("inode no:%d inode addr:%d", ctrl->imap_entry.inode_no, \
			ctrl->imap_entry.inode_addr);
#endif
	ss->version = ctrl->inode.ctime;
	ss->ime.inode_no = ctrl->imap_entry.inode_no;
	ss->ime.inode_addr = ctrl->imap_entry.inode_addr; 
	
	ret = dump_ss(ctrl->storage, ss, 0);

	if (ret < 0) {
		HLOG_ERROR("dump ss error");
		g_message("dump ss error");
		g_mutex_unlock(ctrl->hlfs_access_mutex);
		return ret;
	}
	
	HLOG_DEBUG("leave func %s", __func__);
	g_free(ss);
	g_mutex_unlock(ctrl->hlfs_access_mutex);
	return ret;
}

int rm_snapshot(const char *uri, const char *ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	struct back_storage *storage = init_storage_handler(uri);
	
	if (0 > (ret = append_ss_delmark(storage, ss_name))) {
		HLOG_ERROR("append ss delmark error");
		return ret;
	}
	
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

void list_key(gpointer data, gpointer usr_data)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (data == NULL) {
		HLOG_DEBUG("data is NULL");
#if 0
		g_message("data is NULL");
#endif
		return;
	}
	char *tmp;
	tmp = (char *)g_malloc0(128);
	tmp = g_strconcat(data, "\n", NULL);
	g_strlcat(usr_data, tmp, MAX_BUFSIZE);
	g_free(tmp);
	HLOG_DEBUG("leave func %s", __func__);
}

int list_all_snapshot(const char *uri, char **ss_name_array)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	char *tmp_buf = NULL;
	tmp_buf = (char *)g_malloc0(MAX_BUFSIZE);
	*ss_name_array = tmp_buf;

	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, \	
			g_str_equal, NULL, NULL);
	struct back_storage *storage = init_storage_handler(uri);
#if 0
	g_message("run here");
#endif
	ret = load_all_ss(storage, ss_hashtable);
#if 0
	g_message("run here");
#endif

	if (ret < 0) {
		g_message("load all ss error: %d", ret);
		return ret;
	}
	GList *list = g_hash_table_get_keys(ss_hashtable);
	if (list == NULL)
		g_message("list NULL");
	ret = load_all_ss(storage, ss_hashtable);
	g_list_foreach(list, list_key, *ss_name_array);
	
	HLOG_DEBUG("buf:%s", *ss_name_array);
	if (*ss_name_array == NULL) {
		HLOG_ERROR("buf is NULL");
		return -1;
	}

	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}

int find_inode_by_name(const char *uri, const char *ss_name, \
		uint64_t *inode_addr)
{
	HLOG_DEBUG("enter func %s", __func__);
	struct back_storage *storage = init_storage_handler(uri);
	struct snapshot *ss;
	int ret = 0;
	
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));

	if (0 > (ret = load_ss_by_name(storage, ss, ss_name))) {
		HLOG_ERROR("load ss by name error");
		return ret;
	}
	
	*inode_addr = ss->ime.inode_addr;
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

int get_inode_info(const char *uri, uint64_t inode_addr, \
		uint64_t *creat_time, uint64_t *length) 
{
	HLOG_DEBUG("enter func %s", __func__);
	struct back_storage *storage = init_storage_handler(uri);

/*read superblock. Get SEGMENT_SIZE_SHIFT ... used by load_inode*/
	struct super_block *sb = (struct super_block *)g_malloc0(sizeof(struct super_block));
	if (0 > read_fs_superblock(storage, sb)) {
		g_message("read fs superblock error");
		return -1;
	}
#if 0
	g_message("%llu - inode_addr", inode_addr);
#endif 
	struct inode *inode = load_inode(storage, inode_addr);

	if (inode == NULL)
		return -1;
#if 0
	g_message("%llu %llu", inode->ctime, inode->length);
#endif
	*creat_time = inode->ctime;
	*length = inode->length;

	HLOG_DEBUG("leave func %s", __func__);
	g_free(inode);
	g_free(sb);
	return 0;
}

int hlfs_open_by_inode(HLFS_CTRL *ctrl, uint64_t inode_addr, int flag)
{
	HLOG_DEBUG("enter func %s", __func__);
	
	if (ctrl == NULL || (flag != 0 && flag != 1)) {
		HLOG_ERROR("error parameters: flag should be 0 or 1");	
		return -1;
	}
	if (ctrl->usage_ref > 0) {
		HLOG_DEBUG("Opened by other");
		return -2;
	}
	
	struct inode *inode_tmp = load_inode(ctrl->storage, inode_addr);
	if (inode_tmp == NULL) {
		HLOG_ERROR("load inode error");
		return -3;
	}
	memcpy(&ctrl->inode, inode_tmp, sizeof(struct inode));

	ctrl->usage_ref++;
	HLOG_DEBUG("leave func %s", __func__);
	g_free(inode_tmp);
	return 0;
}

int find_segfile_contain_inode(struct back_storage *storage, \
		uint64_t time, uint32_t *segno)
{
#if 0
	HLOG_DEBUG("enter func %s", __func__);
	int num_entries = 0;
	int i;
	bs_file_info_t *infos = storage->bs_file_list_dir(storage, \
			".", &num_entries);
	if (infos == NULL) 
		return -1;
	bs_file_info_t *info = infos; 
	for (i = 0;;i++) {
		if (!g_str_has_suffix(info->name, "seg")) {
			info++;
			continue;
		}
		if (info->lmtime > time) {
			break;
		}
		if (i == num_entries - 1)
			break;
		info++;
	}
	
	if (info->name == NULL) 
		return -2;
	const char *basename = g_basename(info->name);
	char **v = g_strsplit(basename, ".", 2);
	*segno = atol(v[0]);
	g_strfreev(v);

	HLOG_DEBUG("leave func %s", __func__);
	if (info->lmtime == time) 
		return 1;
	return 0;
#endif
}

int find_inode_in_seg(struct back_storage *storage,  uint32_t segno, \
		uint64_t time, uint64_t *inode_addr)
{
#if 0
	HLOG_DEBUG("enter func %s", __func__);
	char segfile_name[SEGMENT_FILE_NAME_MAX];
	int offset = 0;
	int count;
	struct log_header *lh = NULL;
	struct inode_map_entry *ime = NULL;
	char *buf = NULL;
	
	buf = (char *)g_malloc0(SEGMENT_SIZE);
	build_segfile_name(segno, segfile_name);
	
	bs_file_t file = storage->bs_file_open(storage, segfile_name, BS_READONLY);
	count = storage->bs_file_pread(storage, file, buf, SEGMENT_SIZE, 0);
	storage->bs_file_close(storage, file);
	
	while (offset < count) {
		lh = (struct log_header *)(buf + offset);
		if (lh->ctime > time)
			break;
		offset += lh->log_size;
	}

	ime = (struct inode_map_entry*)(lh + lh->log_size \
			- sizeof(struct inode_map_entry));
	*inode_addr = ime->inode_addr;
	HLOG_DEBUG("leave func %s", __func__);
	g_free(buf);
	return 0;
#endif
}

int find_inode_before_time(const char *uri, uint64_t time, \
		uint64_t *inode_addr)
{
#if 0
	HLOG_DEBUG("enter func %s", __func__);
	struct back_storage *storage = init_storage_handler(uri);
	uint32_t segno = 0;
	int ret = 0;
	
	if (1 == (ret = find_segfile_contain_inode(storage, time, &segno))) {
		*inode_addr = get_last_inode_storage_addr_in_seg(storage, segno);
		return 0;
	}

	if (ret < 0) 
		return ret;

	if (0 > find_inode_in_seg(storage, segno, time, inode_addr)) {
		return -3;
	}

	HLOG_DEBUG("leave func %s", __func__);
	return 0;
#endif
}
