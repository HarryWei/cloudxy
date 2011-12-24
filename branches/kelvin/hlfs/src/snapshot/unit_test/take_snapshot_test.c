#include "api/hlfs.h"

main()
{
	char *uri = "local:///tmp/testenv/testfs";

	g_message("run here");
	HLFS_CTRL *ctrl = init_hlfs(uri);
	const char *name = "snapshot9527";
	g_message("%s", name);
	take_snapshot(ctrl, name);
	g_message("%s", name);
	deinit_hlfs(ctrl);
	return 0;
}
