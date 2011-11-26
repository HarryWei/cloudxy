#include "glib.h"
#include "_hlfs_ctrl.h"

#define SEGSIZE		(1024llu * 1024llu * 64llu)


int main(int argc, char **argv)
{
   g_print(" test init hlfs >>> \n");
   const char * uri = "local:///tmp/testenv";
   const char * fs_name = "testfs";
   struct _hlfs_ctrl * ctrl = init_hlfs(uri,fs_name);
   g_assert(ctrl != NULL);
   g_print(" test  hlfs open >>> \n");
   int ret = 0;
   ret = hlfs_open(ctrl,1);
   g_assert(ret == 0);
   g_print(" test  hlfs write >>> \n");
   static char content[SEGSIZE];
   int offset = 0;
   ret = hlfs_write(ctrl,content,SEGSIZE,offset);
   printf("ret1 is: %d\n", ret);
   ret = hlfs_write(ctrl,content,0,offset);
   printf("ret2 is: %d\n", ret);
   return 0;
}
