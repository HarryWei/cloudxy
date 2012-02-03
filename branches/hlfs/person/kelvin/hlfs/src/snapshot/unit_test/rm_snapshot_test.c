#include "api/hlfs.h"

main()
{
	const char *uri = "local:///tmp/testenv/testfs";
	const char *ss_name = "snapshot3";
	rm_snapshot(uri, ss_name);
	return 0;
}
