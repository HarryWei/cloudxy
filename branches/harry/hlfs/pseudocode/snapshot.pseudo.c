int get_inode_info(const char *uri, uint64_t inode_addr, uint64_t *create_time, uint64_t *length)
{
	storage = init_storage_handler(uri);
	struct inode *inode = load_inode(storage, inode_addr);
	*create_time = inode->ctime;
	*length = inode->length;
	return 0;
}

int hlfs_open_by_inode(struct hlfs_ctrl *ctrl, uint64_t inode_addr, int flag)
{
}

int list_all_snapshot(const char *uri, char *ss_name_array)
{
	storage = init_storage_handler(uri);
	bs_file_t file = storage->open(storage, CHECKPOINT_FILE, BS_READONLY);
	ss_name_array = storage->pread(storage, file, lstat_size, 0);
	for (i = 0; i < snapshot_num; i++) {
		g_strlcpy(ss_name_array, sname);
		ss_name_array += sname_len;
	}
	return 0;
}
