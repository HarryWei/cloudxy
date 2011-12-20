#include "snapshot.h"

int main()
{
	struct snapshot ss = {
		.version = 232325,
		.ime.inode_no = 77,
		.ime.inode_addr = 2324234,
	};
	
	char *uri = "local:///tmp/testenv/testfs";
	struct back_storage *storage = init_storage_handler(uri);
	sprintf(ss.ss_name, "snapshot");
	sprintf(ss.up_ss_name, "up_snapshot");

	if (0 > dump_ss(storage, &ss)) {
		g_message("dump error\n");
		return -1;
	}

	g_message("snapshot write successfully\n");
	return 0;
}
