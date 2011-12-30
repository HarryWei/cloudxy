/*
 *  hlfs/src/snapshot/unittest/test_hlfs_find_get_inode_info.c
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

#define REQ_SIZE 4096
#define TOTAL_SIZE 40960

typedef struct {
	struct hlfs_ctrl *ctrl;
	uint64_t inode_addr1;
	uint64_t inode_addr2;
	uint64_t inode_addr3;
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
test_hlfs_find_inode_before_time(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = (const char *) data;
	int ret = 0;
	uint64_t cur_time = 0;
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
hlfs_get_inode_info_setup(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = (const char *)data;
	fixture->ctrl = init_hlfs(uri);
	int ret = hlfs_open(fixture->ctrl, 1);
	g_assert_cmpint(ret, == , 0);
	g_assert(fixture->ctrl != NULL);
	take_snapshot(fixture, data);
	test_hlfs_find_inode_before_time(fixture, data);
	g_message("leave func %s", __func__);
	return ;
}

static void 
test_hlfs_get_inode_info(Fixture *fixture, const void *data) {
	g_message("enter func %s", __func__);
	const char *uri = (const char *)data;
	uint64_t ctime = 0;
	uint64_t length = 0;
	int ret = hlfs_get_inode_info(uri, fixture->inode_addr1, &ctime, &length);
	g_assert(ret == 0);
	g_message("ctime [%llu], length [%llu]", ctime, length);
	ctime = 0;
	length = 0;
	ret = hlfs_get_inode_info(uri, fixture->inode_addr2, &ctime, &length);
	g_assert(ret == 0);
	g_message("ctime [%llu], length [%llu]", ctime, length);
	ctime = 0;
	length = 0;
	ret = hlfs_get_inode_info(uri, fixture->inode_addr3, &ctime, &length);
	g_assert(ret == 0);
	g_message("ctime [%llu], length [%llu]", ctime, length);
	g_message("leave func %s", __func__);
}

static void 
hlfs_get_inode_info_tear_down(Fixture *fixture, const void *data) {
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
	g_test_add("/misc/hlfs_find_inode_before_time", 
				Fixture, 
				"local:///tmp/testenv/testfs",
				hlfs_get_inode_info_setup, 
				test_hlfs_get_inode_info, 
				hlfs_get_inode_info_tear_down);
	g_message("leave func %s", __func__);
	return g_test_run();
}
