/*
 * Test cache for HFLS;
 * 
 * By Kelvin Wang <senwang@linux.vnet.ibm.com> (c) 2012
 */

#include <glib.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h> 
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include "cache.h"
#include "comm_define.h"
#include "api/hlfs.h"

#define BLK_NO 200

typedef struct {
    HLFS_CTRL *ctrl;
	char *config_file;
} Fixture;

Fixture fixture;
char *hlfs_path = "local:///tmp/testenv/testfs";

int gen_conf_file(char *path, gboolean enable, char *storage_uri, uint64_t block_size, \
		uint32_t cache_size, uint32_t flush_interval, uint32_t flush_trigger_level, \
		uint32_t flush_once_size)
{
	g_message("enter func : %s", __func__);

	if (path == NULL) {
		g_message("path NULL");
		return -1;
	}
	if (flush_trigger_level > 100) {
		g_message("cache flush_trigger_level can not > 100"); 
		return -1;
	}
	GKeyFile *key_file = g_key_file_new();
    g_key_file_set_string(key_file, "STORAGE", "storage_uri", storage_uri);
    g_key_file_set_boolean(key_file, "CACHE", "is_enable_cache", enable);
    g_key_file_set_uint32(key_file, "CACHE", "block_size", block_size);
    g_key_file_set_uint32(key_file, "CACHE", "cache_size", cache_size);
	g_key_file_set_uint32(key_file, "CACHE", "flush_interval", flush_interval);
	g_key_file_set_uint32(key_file, "CACHE", "flush_trigger_level", flush_trigger_level);
	g_key_file_set_uint32(key_file, "CACHE", "flush_once_size", flush_once_size);
	gchar *data = g_key_file_to_data(key_file, NULL, NULL);
	g_message("path:%s", path);
	int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
	if (fd < 0) {
		g_message("open error");
		return -1;
	}
	size_t size = strlen(data);
	g_message("size: %d", size);
	if (0 > write(fd, data, size)) {
		g_message("write file error");
		return -1;
	}
	close(fd);
	return 0;
}

void case_setup()
{
	system("rm -rf /tmp/testenv");
	system("mkdir /tmp/testenv");
	system("cd ../../../build && ./build_local.sh");
	fixture.config_file = (char *)g_malloc(20);
	sprintf(fixture.config_file, "./hlfsrc");
	int ret = 0;
	ret = gen_conf_file(fixture.config_file, TRUE, hlfs_path, 8192, 1024, 10, 80, 100);
	g_assert(ret == 0);
	g_message("gen config file succ");
	fixture.ctrl = init_hlfs_by_config(fixture.config_file);
	g_message("init hlfs done");
	g_assert(fixture.ctrl != NULL);
	if (fixture.ctrl == NULL) {
		g_message("fixture.ctrl NULL");
		return;
	}
	g_assert(fixture.ctrl->cctrl != NULL);
    g_assert(fixture.ctrl != NULL);
	g_message("setup done");
}

void test_cache()
{
/*---init HLFS with cache on by conf file---*/
	int ret = 0;
	g_message("test begin...");
	g_assert(fixture.ctrl->cctrl->block_size == 8192);
	g_assert(fixture.ctrl->cctrl->cache_size == 1024);
	g_assert(fixture.ctrl->cctrl->flush_interval == 10);
	g_assert(fixture.ctrl->cctrl->flush_trigger_level == 80);
	g_assert(fixture.ctrl->cctrl->flush_once_size == 100);
	ret = g_trash_stack_height(&(fixture.ctrl->cctrl->block_cache));
	g_message("height of cache blocks:%d", ret);
	g_assert(ret == 1024);
	g_assert(fixture.ctrl->cctrl->dirty_block != NULL);
    g_assert(g_queue_get_length(fixture.ctrl->cctrl->dirty_block) == 0);
	g_assert(fixture.ctrl->cctrl->block_map != NULL);
	g_assert(fixture.ctrl->cctrl->flush_waken_cond != NULL);
	g_message("initializing HLFS successfully");

/*---Write something and check when flush---*/
	uint64_t pos = 0;
	int loop = 0;
	char *buf = (char *)g_malloc0(8192);
	if (0 > hlfs_open(fixture.ctrl, 1)) {
		g_message("open HLFS error");
		return;
	}
	struct stat *_stat = (struct stat *)g_malloc0(sizeof(struct stat));
	for (loop = 0; loop < BLK_NO; loop++) {
		if (0 > hlfs_write(fixture.ctrl, buf, BLK_NO, pos)) {
			g_message("hlfs_write error");
			return;
		}
		g_message("write the %dth blk", loop);
		pos += BLK_NO;
		if (!g_file_test("/tmp/testenv/testfs/0.seg", G_FILE_TEST_EXISTS)) {
			g_message("file not exists");
		} else {
			memset(_stat, 0, sizeof(_stat));
			ret = stat("/tmp/testenv/testfs/0.seg", _stat);
			if (ret < 0) {
				g_message("error no: %d", errno);
				perror("stat error:");
			}
			g_assert(ret == 0);
			g_message("size of 0.seg:%d", _stat->st_size);
			g_message("inode of 0.seg:%d", _stat->st_ino);
			g_message("atime of 0.seg:%d", _stat->st_atime);
		//sleep(1);
		}
	}
}

void case_teardown()
{
	int ret = 0;
	g_message("going out...");
	ret = hlfs_close(fixture.ctrl); 
	struct stat *_stat = (struct stat *)g_malloc0(sizeof(struct stat));
	ret = stat("/tmp/testenv/testfs/0.seg", _stat);
	g_assert(ret == 0);
	g_message("size of 0.seg:%d", _stat->st_size);
	g_message("inode of 0.seg:%d", _stat->st_ino);
	g_message("atime of 0.seg:%d", _stat->st_atime);
		//sleep(1);
	ret = deinit_hlfs(fixture.ctrl);
	system("rm -rf /tmp/testenv");
	system("rm ./hlfsrc");
	g_assert(ret == 0);
}


int main(int argc, char **argv) {
	if (log4c_init()) {
		g_message("log4c init error!");
	}
	g_test_init(&argc, &argv, NULL);
	g_test_add("/misc/cache", 
				Fixture, 
				NULL,
				case_setup, 
				test_cache, 
				case_teardown);
	return g_test_run();
}
