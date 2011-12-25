#include "snapshot.h"

main()
{
	struct snapshot *ss;
	int flag = -1;
	ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
	const char *buf = "-snapshot324#snapshot323#54561#156#15\n";
	const char *buf1 = "+snapshot324#snapshot323#54561#156#15\n";
	if (0 > load_ss_from_text(ss, buf, &flag)) {
		g_message("error-load ss from text");
		return -1;
	}
	g_message("%llu\n%s\n%s\n%llu\n%llu\n", ss->version, ss->ss_name, \
			ss->up_ss_name, ss->ime.inode_no, ss->ime.inode_addr);
	g_message("flag : %d", flag);
	if (0 > load_ss_from_text(ss, buf1, &flag)) {
		g_message("error-load ss from text");
		return -1;
	}
	g_message("flag : %d", flag);
	g_free(ss);
	return 0;
}
