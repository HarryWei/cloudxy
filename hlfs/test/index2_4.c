/*
 *  test5.c
 *
 *  Harry Wei <harryxiyou@gmail.com> (C) 2011
 */

#include <glib.h>
#include "_hlfs_ctrl.h"
#include <string.h>
/*
 * Close the lhdfs execution and then restart. Invoke the hlfs_read
 * read the position of  datas.
 * Check: read the position of 512k with the length of 64b, check
 *        if the datas are right.
 */

int main(void)
{
	int i;
	g_print("test 5 >>> \n");
	char content2[16*1024+20];
	char content[10];
	memset(content2,'a',16*1024+20);
	const char *uri = "local:///tmp/testenv";
	const char *fs_name = "testfs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);
	
	g_assert(ctrl != NULL);
	g_print("test hlfs open >>> \n");
	int ret = 0;
	ret = hlfs_open(ctrl, 1);
	g_assert(ret == 0);
	g_print("test hlfs write >>>");
	ret = hlfs_write(ctrl, content2, 16lu * 1024lu + 20lu, 15llu * 8llu * 1024llu + 512llu);
	g_assert(ret>0);
	g_print("test hlfs read >>> \n");
	ret = hlfs_read(ctrl, content, 10lu, 15llu * 8llu * 1024llu + 512llu);
	g_assert(ret > 0);
	for (i = 0; i < 10; i++) {
                g_assert(content[i] == 'a');
        }

	ret = hlfs_read(ctrl, content, 10lu, 16llu * 8llu * 1024llu);
	g_assert(ret>0);
	for (i = 0; i < 10; i++) {
                g_assert(content[i] == 'a');
        }
	ret = hlfs_read(ctrl,content,10,17*8*1024+2);
	g_assert(ret>0);
	for (i = 0; i < 10; i++) {
		g_assert(content[i] == 'a');
	}
	return 0;
}
