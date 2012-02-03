#define SNAPSHOT_FILE		"SNAPSHOT.txt"

struct snapshot {
	uint64_t timestamp;		// cp create time
	uint64_t inode_addr;	// inode's addr
	char sname[79];			// cp name
};

struct seg_info {
	uint32_t segno;			// segment number
	uint32_t time;			// the mtime in the last inode of segno.
};

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
{ // This api has been done, and is ok for me (tested)
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

int take_snapshot(struct hlfs_ctrl *ctrl, const char *ssname)
{ // This api has been done, and is ok for me (tested)
	struct inode *cur_inode = load_latest_inode(ctrl->storage);
	struct checkpoint *cp = g_malloc0(sizeof(struct checkpoint));
	g_strlcpy(cp->name, ssname, SNAME_LEN);
	cp->inode_addr = get_last_inode_storage_addr_in_seg(ctrl->storage, ctrl->last_segno);
	cp_2text(cp, cptext);
	dump_snapshot_text(ctrl, cptext, CHECKPOINT_FILE);
	return 0;
}

int rm_snapshot(const char *uri, const char *ssname)
{ // This api has been done, and is ok for me (tested)
	struct back_storage *storage = init_storage_handler(uri);
	//char *tmp_buf = get_all_cp_contents(storage, CHECKPOINT_FILE);
	// Find the ssname (Hashtable), and change the status value to 1.
	//rm_by_ssname(tmp_buf, ssname);
	snapshot_delmark2text(ssname, tmp_buf);
	dump_delfile(storage, tmp_buf);
	return 0;
}

int find_inode_by_name(const char *uri, const char *ssname, uint64_t *inode_addr)
{ // This api has been done, and is ok for me (tested)
	struct back_storage *storage = init_storage_handler(uri);
	char *tmp_buf = get_all_cp_contents(storage, CHECKPOINT_FILE);
	// We can use hash table to find it, O(1).
	find_inode_addr(tmp_buf, ssname, inode_addr);
	return 0;
}
