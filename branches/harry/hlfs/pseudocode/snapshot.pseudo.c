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

int find_inode_before_time(const char *uri, uint64_t timestamp, uint64_t *inode_addr)
{
	struct back_storage *storage = init_storage_handler(uri);
	// seg_info has two paras. 1, segno 2, time: the last inode's mtime
	struct seg_info *infos = get_last_inode_time_in_segs(storage);
	// sort infos in the light of time
	sort(storage, infos);
	// binary-search find the seg
	find(infos, timestamp);
	// find the inode_addr in the seg
	get_inode_addr_by_time(storage, timestamp, info->segno, inode_addr);
}
