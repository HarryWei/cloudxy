#include "api/hlfs.h"
#include "snapshot.h"

#define SS_NAME "snapshot1"
#define LEN 50

int main(int argc, char **argv)
{
	struct back_storage *storage = init_storage_handler("local:///tmp/testenv/testfs");
	const char *ss_name = SS_NAME;
	append_ss_delmark(storage, ss_name);
	append_ss_delmark(storage, "snapshot3");
	return 0;
}
