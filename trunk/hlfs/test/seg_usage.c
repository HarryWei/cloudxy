/*
 *  harry/test/seg_usage.c
 *
 *  Harry Wei <harryxiyou@gmail.com> 2011 (C)
 */

#include <glib.h>
#include <stdint.h>
#include <stdlib.h>

#include "_hlfs_ctrl.h"
//#include "segment_cleaner.h"
#include "storage_helper.h"

#define LEN		(1024llu * 1024llu * 60llu)
#define COUNT		(10llu)

int main(int argc, char **argv)
{
	uint64_t i = 0llu;
	uint64_t j = 0;
	uint64_t ret1 = 0llu;
	uint64_t ram = LEN;
	static char ch[LEN];
	const char *uri = "local:///tmp/testenv";
	const char *fs_name = "testfs";
	struct _hlfs_ctrl *ctrl = init_hlfs(uri, fs_name);

	g_assert(ctrl != NULL);
	int ret = hlfs_open(ctrl, 1);
	g_assert(0 == ret);
	for (i = 0; i < COUNT; i++) {
		for (j = 0; j < LEN; j++) {
			ch[j] = i;
		}
		ret1 = hlfs_write(ctrl, ch, LEN, 0);
		g_assert(ret1 == LEN);
		ret1 = 0;
	}
	return 0;
}
