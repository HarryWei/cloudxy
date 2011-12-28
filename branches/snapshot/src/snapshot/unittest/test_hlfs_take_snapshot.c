/*
 *  hlfs/src/snapshot/unittest/test_hlfs_take_snapshot.c
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
take_snapshot_setup(Fixture *fixture, const void *data) {
	const char *uri = (const char *)data;
	fixture->ctrl = init_hlfs(uri);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert_cmpint(ret, == , 0);
	g_assert(fixture->ctrl != NULL);
	return ;
}

static void 
do_snapshot(Fixture *fixture, int i) {
	char buffer[128];
	memset(buffer, 0, 128);
	if (0 == i) {
		sprintf(buffer, "%s%d", "snapshot", i);
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (1 == i) {
		sprintf(buffer, "%s", " ");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (2 == i) {
		sprintf(buffer, "%s", "+");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (3 == i) {
		sprintf(buffer, "%s", "##@");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (4 == i) {
		sprintf(buffer, "%s", "..");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (5 == i) {
		sprintf(buffer, "%s", " **");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	} else if (6 == i) {
		sprintf(buffer, "%s", "1234");
		int ret = hlfs_take_snapshot(fixture->ctrl, buffer);
		g_assert(ret == 0);
	}
	return ;
}

static void 
test_take_snapshot(Fixture *fixture, const void *data) {
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
	return;
}

static void 
take_snapshot_tear_down(Fixture *fixture, const void *data) {
	hlfs_close(fixture->ctrl);
	deinit_hlfs(fixture->ctrl);
	return;
}

int main(int argc, char **argv) {
	g_test_init(&argc, &argv, NULL);
	g_test_add("/tmp/unittest", 
				Fixture, 
				"local:///tmp/testenv/testfs",
				take_snapshot_setup, 
				test_take_snapshot, 
				take_snapshot_tear_down);
	return g_test_run();
}
