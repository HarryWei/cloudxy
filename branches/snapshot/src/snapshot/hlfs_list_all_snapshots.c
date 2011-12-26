#include "snapshot.h"

#define MAX_BUFSIZE 1024

void list_key(gpointer data, gpointer usr_data)
{
	HLOG_DEBUG("enter func %s", __func__);
	if (data == NULL) {
		HLOG_DEBUG("data is NULL");
#if 0
		g_message("data is NULL");
#endif
		return;
	}
	char *tmp;
	tmp = (char *)g_malloc0(128);
	tmp = g_strconcat(data, "\n", NULL);
	g_strlcat(usr_data, tmp, MAX_BUFSIZE);
	g_free(tmp);
	HLOG_DEBUG("leave func %s", __func__);
}

int hlfs_list_all_snapshots(const char *uri, char **ss_name_array)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	char *tmp_buf = NULL;
	tmp_buf = (char *)g_malloc0(MAX_BUFSIZE);
	*ss_name_array = tmp_buf;

	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, \	
			g_str_equal, NULL, NULL);
	struct back_storage *storage = init_storage_handler(uri);
#if 0
	g_message("run here");
#endif
	ret = load_all_ss(storage, ss_hashtable);
#if 0
	g_message("run here");
#endif

	if (ret < 0) {
		g_message("load all ss error: %d", ret);
		return ret;
	}
	GList *list = g_hash_table_get_keys(ss_hashtable);
	if (list == NULL)
		g_message("list NULL");
	ret = load_all_ss(storage, ss_hashtable);
	g_list_foreach(list, list_key, *ss_name_array);
	
	HLOG_DEBUG("buf:%s", *ss_name_array);
	if (*ss_name_array == NULL) {
		HLOG_ERROR("buf is NULL");
		return -1;
	}

	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}
