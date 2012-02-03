#include "snapshot.h"

main()
{
	struct back_storage *storage = init_storage_handler("local:///tmp/testenv/testfs");
	struct snapshot *ss = (struct snapshot *)g_malloc0(sizeof(struct snapshot));
#if 0
	const char *ss_name2 = "snapshot1";
#endif
#if 1
	const char *ss_name1 = "snapshot5";
#endif
#if 0
	if (0 > load_ss_by_name(storage, ss, ss_name2)) {
		g_message("no such key-value exists");	
		return -1;
	}
#endif 
#if 1
	if (0 > load_ss_by_name(storage, ss, ss_name1)) {
		g_message("no such key-value exists");	
		return -1;
	}
#endif 
	g_message("version: %llu, name:%s, uname:%s, inode_num:%llu, inode_addr:%llu", \
			ss->version, ss->ss_name, ss->up_ss_name, ss->ime.inode_no, \
			ss->ime.inode_addr);
	g_free(ss);
	return 0;
}
