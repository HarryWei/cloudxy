#include "glib.h"
#include "storage_helper.h"
#include "segment_cleaner.h"
#include "_hlfs_ctrl.h"

int main(int argc, char **argv)
{
	int i;
	int num_entries;
	gchar *location = "local:///tmp/testenv";
	gchar *fsname = "testfs";
	struct back_storage *storage = init_storage_handler(location, fsname);

	if (NULL == storage) {
		g_message("can not get storage handler for uri: %s, fsname: %s\n", location, fsname);
		return -1;
	}
	bs_file_info_t *infos = storage->bs_file_list_dir(storage, ".", &num_entries);
	if (NULL == infos) {
		g_message("can not get fs: %s seg entries\n", storage->fs_name);
		return -1;
	}
	bs_file_info_t *info = infos;
	for (i = 0; i < num_entries; i++) {
		g_message("file: %s, size: %d, time: %llu\n", info->name, info->size, info->lmtime);
		info += 1;
	}
	free(infos);
	return 0;
}
