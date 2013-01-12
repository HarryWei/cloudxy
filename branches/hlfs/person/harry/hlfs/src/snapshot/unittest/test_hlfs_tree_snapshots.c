/*
 *  hlfs/src/snapshot/unittest/test_hlfs_tree_snapshots.c
 *  
 *  Harry Wei <harryxiyou@gmail.com> (C) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> 
#include "api/hlfs.h"
#include "hlfs_log.h"
#include "storage.h"
#include "storage_helper.h"

#define REQ_SIZE 4096
#define TOTAL_SIZE 40960

typedef struct {
	struct hlfs_ctrl *ctrl;
	char *uri;
} Fixture;

static void 
hlfs_tree_snapshots_setup(Fixture *fixture, const void *data) {
	const char *test_dir = (const char *)data;
	HLOG_DEBUG("test env dir is %s", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
//	g_assert(g_mkdir(fs_dir, 0700) == 0);
	char *uri = g_malloc0(128);
	g_assert(uri != NULL);
	snprintf(uri, 128, "%s%s", "local://", fs_dir);
//	char *uri = g_build_path(tmp, fs_dir, NULL);
	HLOG_DEBUG("uri is %s", uri);
	pid_t status;
	const char cmd[256];
	memset((char *) cmd, 0, 256);
	sprintf((char *) cmd, "%s %s %s %s %d %s %d %s %d", "../mkfs.hlfs", 
								"-u", uri,
								"-b", 8192,
								"-s", 67108864,
								"-m", 1024);
	HLOG_DEBUG("cmd is [%s]", cmd);
	status = system(cmd);
#if 0
	GKeyFile *sb_keyfile = g_key_file_new();
	g_key_file_set_string(sb_keyfile, "METADATA", "uri", uri);
	g_key_file_set_integer(sb_keyfile, "METADATA", "block_size", 8196);
	g_key_file_set_integer(sb_keyfile, "METADATA", "segment_size", 67108864);
	g_key_file_set_integer(sb_keyfile, "METADATA", "max_fs_size", 671088640);
	gchar *content = g_key_file_to_data(sb_keyfile, NULL, NULL);
	char *sb_file_path = g_build_filename(fs_dir, "superblock", NULL);
	g_print("sb file path is %s\n", sb_file_path);
	GError *error = NULL;
	if (TRUE != g_file_set_contents(sb_file_path, content, strlen(content) + 1, &error)) {
		g_print("error msg is %s", error->message);
		error = NULL;
	}
#endif
	fixture->uri = uri;
	HLOG_DEBUG("fixture->uri is %s", fixture->uri);
	fixture->ctrl = init_hlfs(fixture->uri);
	g_assert(fixture->ctrl != NULL);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
//	g_key_file_free(sb_keyfile);
//	g_free(sb_file_path);
	g_free(fs_dir);
	return ;
}

static void 
do_snapshot(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T1");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T2");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T3");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T4");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T5");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
		HLOG_DEBUG("fixture->ctrl->imap_entry.inode_addr is %llu, inode_no is %llu, \
				iblock is %llu, doubly_iblock is %llu, triply_iblock is %llu", 
				fixture->ctrl->imap_entry.inode_addr, 
				fixture->ctrl->imap_entry.inode_no, 
				fixture->ctrl->inode.iblock, 
				fixture->ctrl->inode.doubly_iblock, 
				fixture->ctrl->inode.triply_iblock);
		int j = 0;
		for (j = 0; j < 12; j++) {
			HLOG_DEBUG("fixture->ctrl->inode.blocks[%d] is %llu", j, fixture->ctrl->inode.blocks[j]);
		}
	} else if (5 == i) {
		sprintf(buffer, "%s", "T6");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T7");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (7 == i) {
		sprintf(buffer, "%s", "T8");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (8 == i) {
		sprintf(buffer, "%s", "T9");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (9 == i) {
		sprintf(buffer, "%s", "T10");
		HLOG_DEBUG("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
do_snapshot1(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T11");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T12");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T13");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T14");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T15");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", "T16");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T17");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (7 == i) {
		sprintf(buffer, "%s", "T18");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (8 == i) {
		sprintf(buffer, "%s", "T19");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (9 == i) {
		sprintf(buffer, "%s", "T20");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
do_snapshot2(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T21");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T22");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T23");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T24");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T25");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", "T26");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T27");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (7 == i) {
		sprintf(buffer, "%s", "T28");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (8 == i) {
		sprintf(buffer, "%s", "T29");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (9 == i) {
		sprintf(buffer, "%s", "T30");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
do_snapshot3(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T31");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T32");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T33");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T34");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T35");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", "T36");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T37");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (7 == i) {
		sprintf(buffer, "%s", "T38");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (8 == i) {
		sprintf(buffer, "%s", "T39");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (9 == i) {
		sprintf(buffer, "%s", "T40");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
do_snapshot4(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T41");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T42");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T43");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T44");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T45");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", "T46");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T47");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (7 == i) {
		sprintf(buffer, "%s", "T48");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (8 == i) {
		sprintf(buffer, "%s", "T49");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (9 == i) {
		sprintf(buffer, "%s", "T50");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
test_hlfs_tree_snapshots(Fixture *fixture, const void *data) {
	char content[REQ_SIZE];
	int offset = 0;
	int i = 0;

	HLOG_DEBUG("Test the main tree snapshot >>>>>>");
	memset(content, 0, REQ_SIZE);
	while (offset <= TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}
	
	HLOG_DEBUG("Test callback and the 1 tree fork >>>>>>");
	hlfs_close(fixture->ctrl);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	uint64_t inode_addr = 0;
	ret = hlfs_find_inode_by_name(fixture->uri, "T5", &inode_addr);
	HLOG_DEBUG("T5's inode_addr is %llu", inode_addr);
	g_assert(ret >= 0);
	ret = hlfs_open_by_inode(fixture->ctrl, inode_addr, 0);
	g_assert(ret >= 0);
	memset(content, 0, REQ_SIZE);
	i = 0;
	offset = 0;
	while (offset <= TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot1(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}

	ret = 0;
	HLOG_DEBUG("Test callback and the 2 tree fork >>>>>>");
	hlfs_close(fixture->ctrl);
	ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	inode_addr = 0;
	ret = hlfs_find_inode_by_name(fixture->uri, "T14", &inode_addr);
	HLOG_DEBUG("T14's inode_addr is %llu", inode_addr);
	g_assert(ret >= 0);
	ret = hlfs_open_by_inode(fixture->ctrl, inode_addr, 0);
	g_assert(ret >= 0);
	memset(content, 0, REQ_SIZE);
	i = 0;
	offset = 0;
	while (offset <= TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot2(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}

	ret = 0;
	HLOG_DEBUG("Test callback and the 3 tree fork >>>>>>");
	hlfs_close(fixture->ctrl);
	ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	inode_addr = 0;
	ret = hlfs_find_inode_by_name(fixture->uri, "T3", &inode_addr);
	HLOG_DEBUG("T3's inode_addr is %llu", inode_addr);
	g_assert(ret >= 0);
	ret = hlfs_open_by_inode(fixture->ctrl, inode_addr, 0);
	g_assert(ret >= 0);
	memset(content, 0, REQ_SIZE);
	i = 0;
	offset = 0;
	while (offset <= TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot3(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}
	
	ret = 0;
	HLOG_DEBUG("Test callback and the 4 tree fork >>>>>>");
	hlfs_close(fixture->ctrl);
	ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	inode_addr = 0;
	ret = hlfs_find_inode_by_name(fixture->uri, "T8", &inode_addr);
	HLOG_DEBUG("T8's inode_addr is %llu", inode_addr);
	g_assert(ret >= 0);
	ret = hlfs_open_by_inode(fixture->ctrl, inode_addr, 0);
	g_assert(ret >= 0);
	memset(content, 0, REQ_SIZE);
	i = 0;
	offset = 0;
	while (offset <= TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot4(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}
	return ;
}

static void 
hlfs_tree_snapshots_tear_down(Fixture *fixture, const void *data) {
	const char *test_dir = (const char *) data;
	HLOG_DEBUG("clean dir path: %s", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
	pid_t status;
#if 0
	const char cmd[256];
	memset((char *) cmd, 0, 256);
	sprintf((char *) cmd, "%s %s %s", "rm", "-r", fs_dir);
	g_message("cmd is [%s]", cmd);
	status = system(cmd);

	struct back_storage *storage = init_storage_handler(fixture->uri);
	g_assert(storage != NULL);
	int nums = 0;
	bs_file_info_t *infos = storage->bs_file_list_dir(storage, ".", &nums);
	g_assert(infos != NULL);
	bs_file_info_t *info = infos;
	int i = 0;
	g_message("nums is %d", nums);
	for (i = 0; i < nums; i++) {
		g_message("info name is %s", info->name);
		char *tmp_file = g_build_filename(fs_dir, info->name, NULL);
		g_message("tmp file name is [%s]", tmp_file);
		g_assert(g_remove(tmp_file) == 0);
		g_free(tmp_file);
		info += 1;
	}
//	char *sb_file = g_build_filename(fs_dir, "superblock", NULL);
//	g_assert(g_remove(sb_file) == 0);
	g_assert(g_remove(fs_dir) == 0);
	g_free(fixture->uri);
	g_free(fs_dir);
//	g_free(sb_file);
	g_free(storage);
	g_free(infos);
#endif
	g_free(fs_dir);
	g_free(fixture->uri);
	hlfs_close(fixture->ctrl);
	deinit_hlfs(fixture->ctrl);
	return;
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_take_snapshot", 
				Fixture, 
				g_get_current_dir(),
				hlfs_tree_snapshots_setup, 
				test_hlfs_tree_snapshots, 
				hlfs_tree_snapshots_tear_down);
	return g_test_run();
}
