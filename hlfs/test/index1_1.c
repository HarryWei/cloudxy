/*
 *  test1.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * use hlfs_wirte write 1000 times with 1k, 4k, 8k's data block.
 * 1k with 0x01, 4k with 0x04, 8k with 0x08. 
 * Check: data has only one segment, can also freely read from hlfs_read.
 */

int main(void)
{
	g_print("test 1 >>> \n");
	const char *uri = "local:///tmp/index1_1env";
	const char *fs_name = "index1_1fs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");

	int i = 0;
	char content1[1024 * 8 * 5];
	for (i = 0; i < 1024 * 8 * 5; i++)
		content1[i] = 0x01;
	ret = hlfs_write(ctrl, content1, 1024lu * 8lu * 5lu, 0llu);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content1, 1024lu * 40lu, 0llu);
	g_assert(ret > 0);
	g_print(" ------------------> %d %d %d\n",content1[0], content1[100], content1[40 * 1024 - 1]);
	return 0;
/*
	int i = 0;
	char content1[1024 * 8 * 5 + 512];
	for (i = 0; i < 1024 * 8 * 5 + 512; i++)
		content1[i] = 0x01;
	ret = hlfs_write(ctrl, content1, 1024 * 8 * 5 + 512, 0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content1, 1024 * 40 + 512, 0);
	g_assert(ret > 0);
	g_print(" ------------------> %d %d %d\n",content1[0], content1[100], content1[40 * 1024 + 512 - 1]);
	return 0;
*/
}
