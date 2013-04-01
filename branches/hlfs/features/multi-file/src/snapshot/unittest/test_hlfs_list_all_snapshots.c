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
	HLFS_CTRL *ctrl;
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

static void test_no_ss_exist(Fixture *fixture, const void *data)
{
	g_message("enter function test_ssfile_not_exist");
	struct back_storage *storage = NULL;
	storage = init_storage_handler(fixture->uri);
	
	if (storage->bs_file_is_exist(storage, SS_FILE) == 0) {
		if (storage->bs_file_delete(storage, SS_FILE) < 0) {
			g_message("snapshot.txt exist but rm failed");
			return;
		}
	}
	
	char *res = NULL;
	int ret = hlfs_list_all_snapshots(fixture->uri, &res);
	g_assert_cmpint(ret, ==, -3);
	
	g_message("leave function test_ssfile_not_exist");
	g_free(res);
}

void test_ss_exist(Fixture *fixture, const void *data) 
{
	if (NULL == fixture->ctrl) {
		g_message("init_hlfs error");
		return;
	}

	if (0 > hlfs_open(fixture->ctrl, 1)) {
		g_message("Open hlfs error");
		return;
	}

	char *content = (char *)g_malloc0(REQ_SIZE);

	int offset = 0;
	int i = 0;
	char *buf = (char *)g_malloc0(128);

	while (offset < TOTAL_SIZE) {
		int ret1 = hlfs_write(fixture->ctrl, content, REQ_SIZE, offset);
		if (ret1 != REQ_SIZE) {
			g_message("write error");
			hlfs_close(fixture->ctrl);
			return;
		}
		offset += REQ_SIZE;
		g_message("offset: %d", offset);

		sprintf(buf, "snapshot%d", i);
		if (0 > hlfs_take_snapshot(fixture->ctrl, buf)) {
			g_message("snapshot %s was taken error", buf);
			hlfs_close(fixture->ctrl);
			return;
		}
		g_message("snapshot %s was taken", buf);
		i++;
	}
	hlfs_close(fixture->ctrl);
	char *res = NULL;
	int ret = hlfs_list_all_snapshots(fixture->uri, &res);
	g_assert_cmpint(ret, >, 0);
	
	g_message("%s", res); 
	g_free(content);
	g_free(buf);
	g_free(res);
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
	Fixture *fixture = NULL;
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_list_all/no_file", 
				Fixture, 
				NULL,
				test_setup, 
				test_no_ss_exist, 
				test_tear_down);
	g_test_add("/misc/hlfs_list_all/have_file", 
				Fixture, 
				NULL,
				test_setup, 
				test_ss_exist,
				test_tear_down);
	return g_test_run();
}
