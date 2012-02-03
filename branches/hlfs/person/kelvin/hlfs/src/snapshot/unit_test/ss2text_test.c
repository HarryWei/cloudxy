#include <stdio.h>
#include "snapshot.h"

main()
{
	struct snapshot *ss;
	int flag;
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	char *buf = (char *)g_malloc0(sizeof(struct snapshot) + 5);
	ss->version = 234;
	sprintf(ss->ss_name, "snapshot9527");
	sprintf(ss->up_ss_name, "snapshotxx");
	ss->ime.inode_no = 77;
	ss->ime.inode_addr = 23424234;
	ss2text(ss, buf, 0);
	g_message("buf: %s", buf);
	ss2text(ss, buf, 1);
	g_message("buf: %s", buf);
	g_free(ss);
	g_free(buf);
	return 0;
}

