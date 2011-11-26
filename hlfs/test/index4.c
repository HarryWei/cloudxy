/*
 *  test5.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */
#include <string.h>
#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * index 4 test case: in one db.
 */

int main(void)
{	
	int i;
	uint64_t v = 8llu * 1024llu * 12llu + 8llu * 1024llu * 1024llu + 8llu * 1024llu * 1024llu * 1024llu + 1024llu;
	g_print("test index 3 >>> \n");
	char content2[64];
	memset(content2,'a',64);
	const char *uri = "local:///tmp/index4env";
	const char *fs_name = "index4fs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");
	ret = hlfs_write(ctrl,content2, 64lu, v);
	g_assert(ret>0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content2, 64lu, v);
	g_assert(ret > 0);
	for (i = 0; i < 64; i++) {
		g_message("------------------> %c",content2[i]);
	}
	return 0;
}
