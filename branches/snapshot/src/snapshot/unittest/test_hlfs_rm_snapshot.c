/*
 *  hlfs/src/snapshot/unittest/test_hlfs_rm_snapshot.c
 *  
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> 
#include "api/hlfs.h"
#include "hlfs_log.h"

#define REQ_SIZE 4096
#define TOTAL_SIZE 40960

typedef struct {
	struct hlfs_ctrl *ctrl;
} Fixture;

static void 
do_snapshot(Fixture *fixture, int i) {
	g_message("enter func %s", __func__);
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s%d", "snapshot", i);
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", " ");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "+");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "##@");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "..");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", " **");
		g_message("%d buffer is [%s]", i, buffer);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "1234");
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
hlfs_rm_snapshot_setup(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = (const char *)data;
	fixture->ctrl = init_hlfs(uri);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert_cmpint(ret, == , 0);
	g_assert(fixture->ctrl != NULL);
	take_snapshot(fixture, data);
	g_message("leave func %s", __func__);
	return ;
}

static void
test_hlfs_rm_snapshot(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = (const char *) data;
	int ret = 0;
	ret = hlfs_rm_snapshot(uri, "snapshot0");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, "1234");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, " ");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, " **");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, "..");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, "+");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, "##@");
	g_assert(ret == 0);
	ret = hlfs_rm_snapshot(uri, "snapshot0");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, "bug here");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, "##@");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, "..");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, " **");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, " ");
	g_assert(ret == 1);
	ret = hlfs_rm_snapshot(uri, "1234");
	g_assert(ret == 1);
	g_message("leave func %s", __func__);
	return ;
}

static void 
hlfs_rm_snapshot_tear_down(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	hlfs_close(fixture->ctrl);
	deinit_hlfs(fixture->ctrl);
	g_message("leave func %s", __func__);
	return;
}

int main(int argc, char **argv) {
	g_message("enter func %s", __func__);
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_rm_snapshot", 
				Fixture, 
				"local:///tmp/testenv/testfs",
				hlfs_rm_snapshot_setup, 
				test_hlfs_rm_snapshot, 
				hlfs_rm_snapshot_tear_down);
	g_message("leave func %s", __func__);
	return g_test_run();
}
