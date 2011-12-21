#include "snapshot.h"

main()
{
	struct snapshot *ss;
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	const char *buf = "54561\nsnapshot324\nsnapshot323\n156\n15";
	if (0 > load_ss_from_text(ss, buf)) {
		g_message("error-load ss from text");
		return -1;
	}
	g_message("%llu\n%s\n%s\n%llu\n%llu\n", ss->version, ss->ss_name, \
			ss->up_ss_name, ss->ime.inode_no, ss->ime.inode_addr);

	g_free(ss);
	return 0;
}
