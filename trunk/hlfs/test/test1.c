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
	const char *uri = "local:///tmp/test1env";
	const char *fs_name = "test1fs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>> \n");
	int i = 0;
	char content1[1024];
	for (i = 0; i < 1024; i++)
		content1[i] = 'a';
	char content2[1024 * 4];
	for (i = 0; i < 1024 * 4; i++)
		content2[i] = 'b';
	char content3[1024 * 8];
	for (i = 0; i < 1024 * 8; i++)
		content3[i] = 'c';
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_write(ctrl, content1, 1024, i * 1024);
		g_assert(ret > 0);
	}
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_write(ctrl, content2, 1024 * 4, 1024 * 1000 + 1024 * 4 * i);
		g_assert(ret > 0);
	}
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_write(ctrl, content3, 1024 * 8, (1024 * 1000 + 1024 * 4 * 1000 + 1024 * 8 * i));
		g_assert(ret > 0);
	}
	g_print("test hlfs read >>> \n");
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_read(ctrl, content1, 1024, i * 1024);
		g_assert(ret > 0);
		g_assert(content1[i] == 'a');
	}
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_read(ctrl, content2, 1024 * 4, 1024 * 1000 + 1024 * 4 * i);
		g_assert(ret > 0);
		g_assert(content2[i] == 'b');
	}
	for (i = 0; i < 1000; i++) {
		ret = 0;
		ret = hlfs_write(ctrl, content3, 1024 * 8, (1024 * 1000 + 1024 * 4 * 1000 + 1024 * 8 * i));
		g_assert(ret > 0);
		g_assert(content3[i] == 'c');
	}
	return 0;
}
