/*
 *  snapshot/unittest/test_hlfs_list_all_snapshots.c
 *  
 *  Kelvin  <kelvin.xupt@gmail.com>
 */

#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h> 
#include "api/hlfs.h"
#include "hlfs_log.h"

#define MAX_BUFSIZE (4 * 1024)
#define REQ_SIZE 4096
#define TOTAL_SIZE 409600
#define URI "local:///tmp/testenv/testfs"
#define SS_NAME "snapshot4"
#define SS_NAME_NOT_EXIST "foo"
#define SS_FILE "snapshot.txt"

void test_ssfile_not_exist()
{
	g_message("enter function test_ssfile_not_exist");
	struct back_storage *storage = NULL;
	storage = init_storage_handler(URI);
	
	if (storage->bs_file_is_exist(storage, SS_FILE) == 0) {
		if (storage->bs_file_delete(storage, SS_FILE) < 0) {
			g_message("snapshot.txt exist but rm failed");
			return;
		}
	}
	
	char *res = NULL;
	res = (char *)g_malloc0(MAX_BUFSIZE);
	int ret = hlfs_list_all_snapshots(URI, &res);
	g_assert_cmpint(ret, ==, -3);
	
	g_message("leave function test_ssfile_not_exist");
	g_free(res);
}

void test_ssfile_exist() 
{
	HLFS_CTRL *ctrl = init_hlfs(URI);
	if (NULL == ctrl) {
		g_message("init_hlfs error");
		return;
	}

	if (0 > hlfs_open(ctrl, 1)) {
		g_message("Open hlfs error");
		return;
	}

	char *content = (char *)g_malloc0(REQ_SIZE);

	int offset = 0;
	int i = 0;
	char *buf = (char *)g_malloc0(128);

	while (offset < TOTAL_SIZE) {
		int ret1 = hlfs_write(ctrl, content, REQ_SIZE, offset);
		if (ret1 != REQ_SIZE) {
			g_message("write error");
			return;
		}
		offset += REQ_SIZE;
		g_message("offset: %d", offset);

		sprintf(buf, "snapshot%d", i);
		if (0 > hlfs_take_snapshot(ctrl, buf)) {
			g_message("snapshot %s was taken error", buf);
			return;
		}
		g_message("snapshot %s was taken", buf);
		i++;
	}
	
	char *res = NULL;
	res = (char *)g_malloc0(MAX_BUFSIZE);
	int ret = hlfs_list_all_snapshots(URI, &res);
	g_assert_cmpint(ret, ==, 0);
	
	g_message("%s", res); 
	g_free(content);
	g_free(buf);
	g_free(res);
	g_assert_cmpint(ret, ==, 0);
}

int main(int argc, char **argv) 
{
	if (log4c_init()) {
		g_message("Initializing log4c error");
		return -1;
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add_func("/list_all/no_file", test_ssfile_not_exist);
	g_test_add_func("/list_all/yes_file", test_ssfile_exist);
	return g_test_run();
}
