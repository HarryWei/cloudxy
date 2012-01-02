/*
 *  hlfs/src/snapshot/unittest/test_hlfs_list_all_snapshots.c
 *  Kelvin <kelvin.xupt@gmail.com> (C) 2011
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
	system("mkdir /tmp/testenv");
	system("cd ../../../../build && ./build_local.sh");
	system("cd -");
	fixture->uri = (char *)g_malloc0(128);
	sprintf(fixture->uri, "local:///tmp/testenv/testfs");
	fixture->ctrl = init_hlfs(fixture->uri);
	return;
}

static void test_find_by_name(Fixture *fixture, const void *data) 
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
	deinit_hlfs(fixture->ctrl);
	g_free(fixture->uri);
	system("rm -rf /tmp/testenv");
	return;
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_find_inode_by_name", 
				Fixture, 
				g_get_current_dir(),
				test_setup, 
				test_find_by_name, 
				test_tear_down);
	return g_test_run();
}
