/*
 * Unit test for hlfs_take_snapshot();
 * cases:
 * case1: We opened hlfs in the mode readonly.
 * case2: ssname has been used.
 * case3: normal.
 * By Kelvin <kelvin.xupt@gmail.com> (c) 2012
 */

#include <glib.h>
#include <stdlib.h>
#include "api/hlfs.h"

#define URI "local:///tmp/testenv/testfs"

typedef struct {
	struct hlfs_ctrl *ctrl;
	char *uri;
} Fixture;

/* 
 * case1 setup: Build the environment for hlfs_take_snapshot().
 * Include:
 * --Format HLFS;
 * --Write something to HLFS;
 * --init and open hlfs readonly;
 */
Fixture case1_fixture;
void case1_setup()
{
	int ret = 0;

	system("mkdir /tmp/testenv");
	system("cd ../../../../build && ./build_local.sh");
	system("cd -");

	case1_fixture.uri = (char *)g_malloc0(MAX_FILE_NAME_LEN);
	sprintf(case1_fixture.uri, URI);
	g_message("uri: %s", case1_fixture.uri);
	case1_fixture.ctrl = init_hlfs(case1_fixture.uri);
	if (case1_fixture.ctrl == NULL) {
		g_message("init_hlfs error");
		g_assert(case1_fixture.ctrl != NULL);
	}
	
	if (0 != (ret = hlfs_open(case1_fixture.ctrl, 1))) {
		g_message("open hlfs error with flag 1");
		exit(EXIT_FAILURE);
	}
	
	char *buf = (char *)g_malloc0(4096);
	if (buf == NULL) {
		g_message("allocate mem error");
		g_free(buf);
		exit(EXIT_FAILURE);
	}
	if (0 > hlfs_write(case1_fixture.ctrl, buf, 4096, 0)) {
		g_message("write buf error");
		g_free(buf);
		exit(EXIT_FAILURE);
	}
	g_message("write buf successfully");
	hlfs_close(case1_fixture.ctrl);

	if (0 != (ret = hlfs_open(case1_fixture.ctrl, 0))) {
		g_message("open hlfs error");
		exit(EXIT_FAILURE);
	}
#if 1
	g_message("seg fault test");
#endif 
}

/*
 * case1 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

void test_case1()
{
	int ret = 0;
	ret = hlfs_take_snapshot(case1_fixture.ctrl, "test_snapshot");
	if (ret == EHLFS_PERM) {
		g_message("errno: %d. Perm deny", ret);
	}
	g_assert_cmpint(ret, ==, EHLFS_PERM);
}

/*
 * case1 teardown:
 * --close hlfs;
 * --deinit hlfs;
 * --free the structure case1_fixture;
 * --rm hlfs root directory;
 */
void case1_teardown()
{
	hlfs_close(case1_fixture.ctrl);
	deinit_hlfs(case1_fixture.ctrl);
	if (case1_fixture.uri != NULL)
		g_free(case1_fixture.uri);
	system("rm -rf /tmp/testenv");
}

int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/hlfs_take_snapshot", 
				Fixture, 
				NULL,
				case1_setup, 
				test_case1, 
				case1_teardown);
	return g_test_run();
}
