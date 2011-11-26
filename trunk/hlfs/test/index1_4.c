/*
 *  test1.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * Check the case of only write in one data block.
 */

int main(void)
{
	g_print("test 1 >>> \n");
	const char *uri = "local:///tmp/index1_4env";
	const char *fs_name = "index1_4fs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");

	int i = 0;
	char content1[8 * 1024 + 1024];
	for (i = 0; i < 9 * 1024; i++)
		content1[i] = 0x01;
	ret = hlfs_write(ctrl, content1, 9lu * 1024lu, 512llu);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content1, 9lu * 1024lu, 512llu);
	g_assert(ret > 0);
	g_print(" ------------------> %d %d %d\n",content1[0], content1[100], content1[9 * 1024 - 1]);
	return 0;
}
