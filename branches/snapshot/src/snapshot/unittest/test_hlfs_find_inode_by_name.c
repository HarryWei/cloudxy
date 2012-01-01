/*
 *  hlfs/src/snapshot/unittest/test_hlfs_list_all_snapshots.c
 *  Kelvin <kelvin.xupt@gmail.com> (C) 2011
 *  test_setup() and test_tear_down() are writen by Harry.
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
#define SS_FILE "snapshot.txt"
#define MAX_BUFSIZE (4 * 1024)

typedef struct {
	struct hlfs_ctrl *ctrl;
	char *uri;
} Fixture;

static void test_setup(Fixture *fixture, const void *data) 
{
	const char *test_dir = (const char *)data;
	g_print("test env dir is %s\n", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
	g_assert(g_mkdir(fs_dir, 0700) == 0);
	char *uri = g_malloc0(128);
	g_assert(uri != NULL);
	snprintf(uri, 128, "%s%s", "local://", fs_dir);
	g_print("uri is %s\n", uri);
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
	fixture->uri = uri;
	g_print("fixture->uri is %s\n", fixture->uri);
	fixture->ctrl = init_hlfs(fixture->uri);
	g_assert(fixture->ctrl != NULL);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert(ret == 0);
	g_key_file_free(sb_keyfile);
	g_free(sb_file_path);
	g_free(fs_dir);
	return;
}

static void test_find_by_name(Fixture *fixture) 
{
	char *content = (char *)g_malloc0(REQ_SIZE);
	int offset = 0;
	int i = 0;
	char *buf = (char *)g_malloc0(128);

	while (offset < TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		g_assert_cmpint(ret1, ==, REQ_SIZE);
		offset += REQ_SIZE;
		g_message("offset : %d", offset);
		sprintf(buf, "snapshot%d", i);
		g_assert(buf != NULL);
		int ret = hlfs_take_snapshot(fixture->ctrl, buf);
		g_assert_cmpint(ret, ==, 0);
		g_message("snapshot %s was taken", buf);
		i++;
	}

	char *tmp_buf = "snapshot4";
	uint64_t inode_addr = 0;

	int res = hlfs_find_inode_by_name(fixture->uri, tmp_buf, &inode_addr);
	g_assert_cmpint(res, ==, 0);
	g_assert_cmpint(inode_addr, !=, 0);
	g_message("inode addr of %s is %llu", tmp_buf, inode_addr);

	g_free(buf);
	g_free(content);
	return;
}

static void test_tear_down(Fixture *fixture, const void *data) 
{
	const char *test_dir = (const char *) data;
	g_print("clean dir path: %s\n", test_dir);
	char *fs_dir = g_build_filename(test_dir, "testfs", NULL);
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
	g_assert(g_remove(fs_dir) == 0);
	g_free(fixture->uri);
	g_free(fs_dir);
	g_free(storage);
	g_free(infos);
	hlfs_close(fixture->ctrl);
	deinit_hlfs(fixture->ctrl);
	return;
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/find_by_name", 
				Fixture, 
				g_get_current_dir(),
				test_setup, 
				test_find_by_name, 
				test_tear_down);
	return g_test_run();
}
