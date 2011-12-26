/*
 *  snapshot/unittest/test_hlfs_find_inode_by_name.c
 *  
 *  Kelvin  <kelvin.xupt@gmail.com>
 */

#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> 
#include "api/hlfs.h"
#include "hlfs_log.h"

#define REQ_SIZE 4096
#define TOTAL_SIZE 409600

typedef struct {
	struct hlfs_ctrl *ctrl;
} Fixture;

static void take_snapshot_setup(Fixture *fixture, const void *data) 
{
	const char *uri = (const char *)data;
	fixture->ctrl = init_hlfs(uri);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert_cmpint(ret, == , 0);
	g_assert(fixture->ctrl != NULL);
	return;
}

static void test_take_snapshot(Fixture *fixture, const void *data) 
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

	int res = hlfs_find_inode_by_name((const char *)data, tmp_buf, &inode_addr);
	g_assert_cmpint(res, ==, 0);
	g_assert_cmpint(inode_addr, !=, 0);
	g_message("inode addr of %s is %llu", tmp_buf, inode_addr);

	g_free(buf);
	g_free(content);
	return;
}

static void take_snapshot_tear_down(Fixture *fixture, const void *data) 
{
	hlfs_close(fixture->ctrl);
	deinit_hlfs(fixture->ctrl);
	return;
}

int main(int argc, char **argv) 
{
	g_test_init(&argc, &argv, NULL);
	g_test_add("/test/listall", Fixture, "local:///tmp/testenv/testfs", \
			take_snapshot_setup, test_take_snapshot, take_snapshot_tear_down);
	return g_test_run();
}
