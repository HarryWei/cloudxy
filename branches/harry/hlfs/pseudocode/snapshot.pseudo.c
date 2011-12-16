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
	struct inode *inode = load_inode(ctrl->storage, inode_addr);
	mempcpy(&ctrl->inode, inode, sizeof(struct inode));
	if (0 == flag) { // the common condition
		ctrl->rw_inode_flag = 0;
	} else if (1 == flag) { // forbid hlfs_write
		ctrl->rw_inode_flag = 1;
	} else {
		g_message("the bad flag for open inode!");
	}
	return 0;
}

int list_all_snapshot(const char *uri, char *ss_name_array)
{
	storage = init_storage_handler(uri);
	//open the cp.txt file
	bs_file_t file = storage->open(storage, CHECKPOINT_FILE, BS_READONLY);
	//store all the cp.txt contents into tmp_buf 
	//lstat_size: the size of cp.txt, which can be got from lstat func
	tmp_buf = storage->pread(storage, file, lstat_size, 0); 
	for (i = 0; i < snapshot_num; i++) {
		sname = g_strsplit(tmp_buf); // split tmp_buf and get the sname
		g_strlcpy(ss_name_array, sname);
		sname_len = strlen(sname);
		ss_name_array += sname_len;
	}
	return 0;
}
