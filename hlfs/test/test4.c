/*
 *  test4.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * use hlfs_wirte 1M data with 0xef, the position of (1M - 30b) write 
 * 64b data with 0xcd.
 * Check: read the position of (1M - 30b) with the length of 64b, check
 *        if the datas are right.
 */

int main(void)
{
	g_print("test 4 >>> \n");
	const char *uri = "local:///tmp/testenv";
	const char *fs_name = "testfs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");
	int i = 0;
	char content1[1024 * 1024];
	for (i = 0; i < 1024 * 1024; i++)
		content1[i] = 0xef;
	ret = hlfs_write(ctrl, content1, 1024, 0);
	g_assert(ret > 0);
	char content2[64];
	for (i = 0; i < 64; i++)
		content2[i] = 0xcd;
	ret = hlfs_write(ctrl, content2, 64, (1024 * 1024 - 30));
	g_assert(ret > 0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content2, 64, (1024 * 1024 - 30));
	g_assert(ret > 0);
	for (i = 0; i < 64; i++) {
		g_assert(content2[i] == 0xcd);
	}
	return 0;
}
