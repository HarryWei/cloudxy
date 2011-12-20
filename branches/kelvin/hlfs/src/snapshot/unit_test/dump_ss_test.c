#include "snapshot.h"

int main()
{
	struct snapshot ss = {
		.version = 232325,
		.ime.inode_no = 77,
		.ime.inode_addr = 2324234,
	};
	sprintf(ss.ss_name, "snapshot");
	sprintf(ss.up_ss_name, "snapshot");
	
	struct snapshot ss1 = {
		.version = 2325,
		.ime.inode_no = 77,
		.ime.inode_addr = 4234,
	};
	sprintf(ss1.ss_name, "snapshot1");
	sprintf(ss1.up_ss_name, "snapshot");

	struct snapshot ss2 = {
		.version = 232,
		.ime.inode_no = 77,
		.ime.inode_addr = 234,
	};
	sprintf(ss2.ss_name, "snapshot2");
	sprintf(ss2.up_ss_name, "snapshot1");
	
	struct snapshot ss3 = {
		.version = 237872,
		.ime.inode_no = 77,
		.ime.inode_addr = 12234,
	};
	sprintf(ss3.ss_name, "snapshot3");
	sprintf(ss3.up_ss_name, "snapshot2");

	struct snapshot ss4 = {
		.version = 237372,
		.ime.inode_no = 77,
		.ime.inode_addr = 34,
	};
	sprintf(ss4.ss_name, "snapshot4");
	sprintf(ss4.up_ss_name, "snapshot3");

	struct snapshot ss5 = {
		.version = 23732,
		.ime.inode_no = 7,
		.ime.inode_addr = 4,
	};
	sprintf(ss5.ss_name, "snapshot5");
	sprintf(ss5.up_ss_name, "snapshot4");

	char *uri = "local:///tmp/testenv/testfs";
	struct back_storage *storage = init_storage_handler(uri);

	if (0 > dump_ss(storage, &ss)) {
		g_message("dump error");
		return -1;
	}
	if (0 > dump_ss(storage, &ss1)) {
		g_message("dump error");
		return -1;
	}
	if (0 > dump_ss(storage, &ss2)) {
		g_message("dump error");
		return -1;
	}
	if (0 > dump_ss(storage, &ss3)) {
		g_message("dump error");
		return -1;
	}
	if (0 > dump_ss(storage, &ss4)) {
		g_message("dump error");
		return -1;
	}
	if (0 > dump_ss(storage, &ss5)) {
		g_message("dump error");
		return -1;
	}

	g_message("snapshot write successfully");
	return 0;
}
