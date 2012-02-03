#include "snapshot.h"

void print_key(gpointer data, gpointer usr_data)
{
	g_message("%s", data);
}

void print_value(gpointer data, gpointer usr_data)
{
	struct snapshot *ss = data;
	printf("%llu\n%s\n%s\n%llu\n%llu\n\n", ss->version, ss->ss_name, \
			ss->up_ss_name, ss->ime.inode_no, ss->ime.inode_addr);
}

main()
{
	if (log4c_init())
		g_message("log4c init error");
	struct back_storage *storage = init_storage_handler("local:///tmp/testenv/testfs");
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal, NULL, NULL);
	load_all_ss(storage, ss_hashtable);
	GList *list = g_hash_table_get_keys(ss_hashtable);
	g_list_foreach(list, print_key, NULL);
	g_list_free(list);
	GList *list1 = g_hash_table_get_values(ss_hashtable);
	g_list_foreach(list1, print_value, NULL);
	g_list_free(list1);
	g_hash_table_destroy(ss_hashtable);
	return 0;
}
