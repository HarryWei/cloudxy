#include "api/hlfs.h"

main()
{
	char *buf = NULL;
	const char *uri = "local:///tmp/testenv/testfs";
	int i;
	list_all_snapshot(uri, &buf);
	g_message("%s", buf);
	return 0;
}
