#include "api/hlfs.h"
#include "snapshot.h"

#define SS_NAME "snapshot1"
#define LEN 50

int main(int argc, char **argv)
{
	if (argc != 2) {
		g_message("arg error");
		return -1;
	}
	struct back_storage *storage = init_storage_handler(argv[1]);
	const char *ss_name = SS_NAME;
	append_ss_delmark(storage, ss_name);
	append_ss_delmark(storage, "snapshot2");
	char buf[LEN];

	bs_file_t file = NULL;
	char *file_name = g_build_filename("/tmp/testenv/testfs", SS_DEL_FILE, NULL);
	g_message("path: %s", file_name);
	file = storage->bs_file_open(storage, SS_DEL_FILE, BS_READONLY);
	if (file == NULL) {
		g_message("Open file error-%m");
		return -1;
	}
	
	if (-1 == storage->bs_file_pread(storage, file, buf, LEN, 0)) {
		g_message("Read file error-%m");
		return -1;
	}

	g_message("buf in file is :\n%s", buf);
	storage->bs_file_close(storage, file);

	return 0;
}
