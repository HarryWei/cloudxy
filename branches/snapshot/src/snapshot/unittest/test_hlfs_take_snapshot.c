/*
 *  snapshot/unittest/test_hlfs_take_snapshot.c
 *
 *  Harry Wei <harryxiyou@gmail.com>
 */
#include <glib.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "snapshot.h"
#include "snapshot_helper.h"
#include "storage_helper.h"

typedef struct {
	struct hlfs_ctrl *ctrl;
} Fixture;

static void
take_snapshot_setup(Fixture *fixture,
					const void *data) {
	const char *uri = (const char *) data;
	fixture->ctrl = init_hlfs(uri);
	g_assert(ctrl != NULL);
	return ;
}

static void
test_take_snapshot(Fixture *fixture,
					const void *data) {
	const char *sname1 = "sname1";
	int ret = hlfs_take_snapshot(fixture->ctrl, sname1);
	g_assert(ret != -1);
	return ;
}

//TODO all the resources established, and then destroy them
static void
take_snapshot_tear_down(Fixture *fixture,
						const void *data) {
	g_free(fixture->ctrl);
	return ;
}

int main(int argc, char *argv[]) {
	g_test_init(&argc, &argv, NULL);
	g_test_add("/tmp/snapshot", 
				Fixture, 
				"local:///tmp/testenv/testfs", 
				take_snapshot_setup, 
				test_take_snapshot,
				take_snapshot_tear_down);
	return g_test_run();
}
