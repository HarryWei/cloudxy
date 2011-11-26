/*
 *  test3.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <stdio.h>
#include <glib.h>
#include "_hlfs_ctrl.h"

/*
 * use hlfs_wirte 1M data with 'a', the position of 512k write 
 * 64b data with 'b'.
 * Check: read the position of 512k with the length of 64b, check
 *        if the datas are right.
 */

int main(void)
{
	g_print("test 3 >>> \n");
	const char *uri = "local:///tmp/test3env";
	const char *fs_name = "test3fs";
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
		content1[i] = 'a';
	ret = hlfs_write(ctrl, content1, 1024 * 1024, 0);
	g_assert(ret > 0);
	char content2[64];
	for (i = 0; i < 64; i++)
		content2[i] = 'b';
	ret = hlfs_write(ctrl, content2, 64, 512 * 1024);
	g_assert(ret > 0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content1, 64, 0);
	g_assert(ret > 0);
	
	printf("in content1: 0-%c, 32-%c, 63-%c\n", content1[0], content1[32], content1[63]);
	printf("in content2: 0-%c, 32-%c, 63-%c\n", content2[0], content2[32], content2[63]);
	return 0;
}
