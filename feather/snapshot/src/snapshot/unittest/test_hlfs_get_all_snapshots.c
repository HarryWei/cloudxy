/*
 *  hlfs/src/snapshot/unittest/test_hlfs_get_all_snapshots.c
 *  
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> 
#include "api/hlfs.h"
#include "hlfs_log.h"
#include "misc.h"
#include "storage.h"
#include "storage_helper.h"
#include "snapshot.h"

#define REQ_SIZE 4096
#define TOTAL_SIZE 40960

typedef struct {
	struct hlfs_ctrl *ctrl;
	uint64_t inode_addr1;
	uint64_t inode_addr2;
	uint64_t inode_addr3;
	char *uri;
} Fixture;

static void 
do_snapshot(Fixture *fixture, int i) {
	g_message("enter func %s", __func__);
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s", "T0");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", "T1");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "T2");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "T3");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "T4");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", "T5");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "T6");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	g_message("leave func %s", __func__);
	return ;
}

static void 
take_snapshot(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	char content[REQ_SIZE];
	int offset = 0;
	int i = 0;

	memset(content, 0, REQ_SIZE);
	while (offset < TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		do_snapshot(fixture, i);
		offset += REQ_SIZE;
		i += 1;
	}
	g_message("leave func %s", __func__);
	return;
}

static void
test_hlfs_find_inode_before_time(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = fixture->uri;
	int ret = 0;
	uint64_t cur_time = get_current_time() - 300;
	uint64_t inode_addr = 0;
	ret = hlfs_find_inode_before_time(uri, cur_time, &inode_addr);
	g_assert(ret == 0);
	fixture->inode_addr1 = inode_addr;
	g_message("current time [%llu], inode addr is [%llu]", cur_time, inode_addr);
	cur_time = get_current_time();
	cur_time -= 400;
	inode_addr = 0;
	ret = hlfs_find_inode_before_time(uri, cur_time, &inode_addr);
	g_assert(ret == 0);
	fixture->inode_addr2 = inode_addr;
	g_message("current time [%llu], inode addr is [%llu]", cur_time, inode_addr);
	cur_time = get_current_time();
	inode_addr = 0;
	ret = hlfs_find_inode_before_time(uri, cur_time, &inode_addr);
	g_assert(ret == 0);
	fixture->inode_addr3 = inode_addr;
	g_message("current time [%llu], inode addr is [%llu]", cur_time, inode_addr);
	g_message("leave func %s", __func__);
	return ;
}

static void 
hlfs_get_all_snapshots_setup(Fixture *fixture, const void *data) {
	const char *test_dir = (const char *)data;
	g_print("test env dir is %s\n", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
//	g_assert(g_mkdir(fs_dir, 0700) == 0);
	char *uri = g_malloc0(128);
	g_assert(uri != NULL);
	snprintf(uri, 128, "%s%s", "local://", fs_dir);
//	char *uri = g_build_path(tmp, fs_dir, NULL);
	g_print("uri is %s\n", uri);
	pid_t status;
	const char cmd[256];
	memset((char *) cmd, 0, 256);
	sprintf((char *) cmd, "%s %s %s %s %d %s %d %s %d", "../mkfs.hlfs", 
								"-u", uri,
								"-b", 8192,
								"-s", 67108864,
								"-m", 1024);
	g_message("cmd is [%s]", cmd);
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
	g_print("fixture->uri is %s\n", fixture->uri);
	fixture->ctrl = init_hlfs(fixture->uri);
	g_assert(fixture->ctrl != NULL);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	take_snapshot(fixture, data);
//	test_hlfs_find_inode_before_time(fixture, data);
//	g_key_file_free(sb_keyfile);
//	g_free(sb_file_path);
	g_free(fs_dir);
	return ;
}

static void 
test_hlfs_get_all_snapshots(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = fixture->uri;
	int num = 0;
	struct snapshot *ss_buf = hlfs_get_all_snapshots(fixture->uri, &num);
	g_assert(ss_buf != NULL);
	int i = 0;
	for (i = 0; i < num; i++) {
		struct snapshot *ss = ss_buf;
		g_message("timestamp is %llu, inode_addr is %llu, sname is %s, up_sname is %s", 
				ss->timestamp, ss->inode_addr,
				ss->sname, ss->up_sname);
		ss_buf += 1;
	}
	g_message("leave func %s", __func__);
}

static void 
hlfs_get_all_snapshots_tear_down(Fixture *fixture, const void *data) {
	const char *test_dir = (const char *) data;
	g_print("clean dir path: %s\n", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
#if 0
	pid_t status;
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
	g_message("enter func %s", __func__);
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_find_inode_before_time", 
				Fixture, 
				g_get_current_dir(),
				hlfs_get_all_snapshots_setup, 
				test_hlfs_get_all_snapshots, 
				hlfs_get_all_snapshots_tear_down);
	g_message("leave func %s", __func__);
	return g_test_run();
}
