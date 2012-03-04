/*
 * Unit test for hlfs_take_snapshot();
 * cases:
 * case1: We opened hlfs in the mode readonly.
 * case2: ssname has been used.
 * case3: normal.
 * case4: tree snapshot test.
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
	if (ret < 0) {
		g_message("errno: Perm deny");
	}
	g_assert_cmpint(ret, <, 0);
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

/*
 * case2 setup:
 * --Format hlfs;
 * --Write something to HLFS;
 * --take snapshot with the name test_snapshot
 * --init and open hlfs writeable;
 */
Fixture case2_fixture;
void case2_setup()
{
	int ret = 0;

	system("mkdir /tmp/testenv");
	system("cd ../../../../build && ./build_local.sh");
	system("cd -");

	case2_fixture.uri = (char *)g_malloc0(MAX_FILE_NAME_LEN);
	sprintf(case2_fixture.uri, URI);
	g_message("uri: %s", case2_fixture.uri);
	case2_fixture.ctrl = init_hlfs(case2_fixture.uri);
	if (case2_fixture.ctrl == NULL) {
		g_message("init_hlfs error");
		g_assert(case2_fixture.ctrl != NULL);
	}
	
	if (0 != (ret = hlfs_open(case2_fixture.ctrl, 1))) {
		g_message("open hlfs error with flag 1");
		exit(EXIT_FAILURE);
	}
	
	char *buf = (char *)g_malloc0(4096);
	if (buf == NULL) {
		g_message("allocate mem error");
		g_free(buf);
		exit(EXIT_FAILURE);
	}
	if (0 > hlfs_write(case2_fixture.ctrl, buf, 4096, 0)) {
		g_message("write buf error");
		g_free(buf);
		exit(EXIT_FAILURE);
	}
	if (0 < (ret = hlfs_take_snapshot(case2_fixture.ctrl, "test_snapshot"))) {
		g_message("take snapshot error when setup case2");
		g_free(buf);
		exit(EXIT_FAILURE);
	} 
				
	
	g_message("write buf successfully");
	hlfs_close(case2_fixture.ctrl);

	if (0 != (ret = hlfs_open(case2_fixture.ctrl, 1))) {
		g_message("open hlfs error");
		exit(EXIT_FAILURE);
	}
}

/*
 * case2 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot";
 *   The return value should be a minus.
 */

void test_case2()
{
	int ret = 0;
	ret = hlfs_take_snapshot(case1_fixture.ctrl, "test_snapshot");
	if (ret < 0) {
		g_message("error: snapshot exists", ret);
	}
	g_assert_cmpint(ret, <, 0);
}

/*
 * case2 teardown:
 * --close hlfs;
 * --deinit hlfs;
 * --free the structure case1_fixture;
 * --rm hlfs root directory;
 */
void case2_teardown()
{
	hlfs_close(case2_fixture.ctrl);
	deinit_hlfs(case2_fixture.ctrl);
	if (case2_fixture.uri != NULL)
		g_free(case2_fixture.uri);
	system("cat /tmp/testenv/testfs/snapshot.txt");
	system("rm -rf /tmp/testenv");
}

/*
 * case3 setup:
 * --Format hlfs;
 * --init and open hlfs writeable;
 */
Fixture case3_fixture;
void case3_setup()
{
	int ret = 0;

	system("mkdir /tmp/testenv");
	system("cd ../../../../build && ./build_local.sh");
	system("cd -");

	case3_fixture.uri = (char *)g_malloc0(MAX_FILE_NAME_LEN);
	sprintf(case3_fixture.uri, URI);
	g_message("uri: %s", case3_fixture.uri);
	case3_fixture.ctrl = init_hlfs(case3_fixture.uri);
	if (case3_fixture.ctrl == NULL) {
		g_message("init_hlfs error");
		g_assert(case3_fixture.ctrl != NULL);
	}
	
	if (0 != (ret = hlfs_open(case3_fixture.ctrl, 1))) {
		g_message("open hlfs error with flag 1");
		exit(EXIT_FAILURE);
	}
}

/*
 * case3 test:
 * --call the function hlfs_take_snapshot with the arguments ctrl & "test_snapshot(i)";
 * we can print snapshot.txt to check wether snapshots were taken properly. 
 */

void test_case3()
{
	int ret = 0;
	char *buf = (char *)g_malloc0(4096);
	uint64_t offset = 0;
	int i = 0;
	char *name_buf = (char *)g_malloc0(MAX_FILE_NAME_LEN);

	while (offset < 40960) {
		if (0 > (ret = hlfs_write(case3_fixture.ctrl, buf, 4096, offset))) {
			g_message("hlfs_write error");
			g_free(buf);
			g_free(name_buf);
			exit(EXIT_FAILURE);
		}
		memset(name_buf, 0, sizeof(name_buf));
		sprintf(name_buf, "test_snapshot_%d", i);
		ret = hlfs_take_snapshot(case3_fixture.ctrl, name_buf);
		if (ret < 0) {
			g_message("error: snapshot exists", ret);
			g_free(buf);
			g_free(name_buf);
			g_assert_cmpint(ret, ==, 0);
		}
		i++;
		offset += 4096;
	}
	g_free(buf);
	g_free(name_buf);
}

/*
 * case3 teardown:
 * --close hlfs;
 * --deinit hlfs;
 * --free the structure case3_fixture;
 * --print snapshot.txt to check result.
 * --rm hlfs root directory;
 */
void case3_teardown()
{
	hlfs_close(case2_fixture.ctrl);
	deinit_hlfs(case2_fixture.ctrl);
	if (case2_fixture.uri != NULL)
		g_free(case2_fixture.uri);
	system("cat /tmp/testenv/testfs/snapshot.txt");
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
	g_test_add("/misc/hlfs_take_snapshot", 
				Fixture, 
				NULL,
				case2_setup, 
				test_case2, 
				case2_teardown);
	g_test_add("/misc/hlfs_take_snapshot", 
				Fixture, 
				NULL,
				case3_setup, 
				test_case3, 
				case3_teardown);
	return g_test_run();
}
