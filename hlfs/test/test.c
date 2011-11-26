#include "glib.h"
#include "api/hlfs.h"
#include <string.h>
#include "hlfs_log.h"

#define LEN		(63ull * 1024ull * 1024ull)

int main(){
	if (log4c_init()) {
		g_message("log4c_init error!");
	}
   g_print(" test init hlfs >>> \n");
   const char * uri = "local:///tmp/testenv/testfs";
//   const char * fs_name = "testfs";
   HLFS_CTRL * ctrl = init_hlfs(uri);
   g_assert(ctrl != NULL);
   g_print(" test  hlfs open >>> \n");
   uint64_t ret = 0;
   ret = hlfs_open(ctrl,1);
   g_assert(ret == 0);
   g_print(" test  hlfs write >>> \n");
   static char content[LEN];
   memset(content, 0, LEN);
#if 0
   int offset = 0;
   while(offset < 8192*10*1000){
      ret = hlfs_write(ctrl,content,8192*10,offset);
      g_assert(ret==8192*10);
      offset +=8192*10;
      printf("offset:%d\n",offset);
}
#endif
	uint64_t offset = 0;
	int i = 0;
	for (i = 0; i < 100; i++) {
      		ret = hlfs_write(ctrl,content,LEN,offset);
      		g_assert(ret==LEN);
      		offset += LEN;
      		printf("offset:%llu\n",offset);
	}
   ret = hlfs_close(ctrl);
   deinit_hlfs(ctrl);
   return 0;
}
