/*
 *  test5.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <string.h>
#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * index 3 test case: in three dbs.
 */

int main(void)
{	
	int i;
	g_print("test index 3 >>> \n");
	char content2[16 * 1024];
	memset(content2,'a',16 * 1024);
	const char *uri = "local:///tmp/index3_1env";
	const char *fs_name = "index3_1fs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");
	ret = hlfs_write(ctrl,content2, 16lu * 1024lu , 8llu * 1024llu * 12llu + 8llu * 1024llu * 1024llu + 1024llu);
	g_assert(ret>0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content2, 16lu * 1024lu, 1024llu * 12llu * 8llu + 8llu * 1024llu * 1024llu + 1024llu);
	g_assert(ret > 0);
	for (i = 0; i < 16 * 1024; i++) {
		g_assert(content2[i] == 'a');
	}
	g_message("--------------------------> %c\n", content2[16 * 1024 - 1]);
	return 0;
}
