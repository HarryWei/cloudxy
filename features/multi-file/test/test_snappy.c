#include <string.h>
#include <stdint.h>
#include "api/hlfs.h"
#include "hlfs_log.h"
#include "glib.h"



#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
/*#include "/home/glen/Downloads/hlfs/3part/snappy/include/snappy-c.h"*/
#include <string.h>
#include <assert.h>


#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "comm_define.h"
#include "misc.h"
#include "address.h"
//#include "snappy-c.h"

#include "storage.h"
#include "icache.h"



static gchar *uri = NULL;
static gint request_size = 0;
static gint total_size = 0;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem location",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri/conf", "FSLOC"},
    	{"filesystem request size", 'r', 0, G_OPTION_ARG_INT, &request_size, "test request size", "REQUESTSIZE"},
    	{"filesystem tatal size", 'a', 0, G_OPTION_ARG_INT, &total_size, "test total request size", "TOTALSIZE"},
    	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
    	{NULL}
};

/******************************************************************************************************************************************/

void my_err(const char *err_string, int line)
{
	fprintf(stderr, "line: %d  ", line);
	perror(err_string);
	exit (1);
}




/******************************************************************************************************************************************/

static void 
error_func(GOptionContext *context, GOptionGroup *group, 
				gpointer data, GError **error) 
{
    if (*error && (*error)->message) {
        gchar *progname = g_get_prgname();
        g_print("%s: %s\nTry '%s --help' for more information.\n", 
				progname, (*error)->message, progname);
        exit(EXIT_FAILURE);
    }
}
#if 1
/*
 * ./test -u uri -r request_size -a total_size
 * uri: /tmp/testenv/testfs
 * request_size: 4096bytes
 * total_size: 409600bytes
 */
int main(int argc, char *argv[]){
    if (log4c_init()) {
        g_message("log4c_init error!");
    }
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- hlfs test -");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            (GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }
	g_option_context_free(context);
    g_print("TEST: uri is %s, request size is %d, total size is %d\n", uri, request_size, total_size);
    char *content = (char*)g_malloc0(request_size);
    HLFS_CTRL * ctrl = init_hlfs(uri);
    g_assert(ctrl != NULL);
    uint64_t ret = 0;
    ret = hlfs_open(ctrl,1);
    g_assert(ret == 0);
    g_print("TEST  hlfs open over \n");
	g_print("test hlfs write\n");
	sleep(2);
    int offset = 0;
    while(offset < total_size){
        ret = hlfs_write(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }
    g_print("TEST  hlfs write over \n");
	g_print("test hlfs read\n");
	sleep(2);
    offset = 0;
    while(offset < total_size){
        ret = hlfs_read(ctrl,content,request_size,offset);
        g_assert(ret==request_size);
        offset +=request_size;
        printf("offset:%d\n",offset);
    }

	g_print("again ------------------------\n");
    offset = 0;
/******************************************************************************************************************************************/



	int fdd;
	int len;
	int rett;
	int i;
	//char read_buf[409600];
	char *contentt = (char*)g_malloc0(40960);

	if((fdd = open("test.c", O_RDONLY )) == -1){
	my_err("open", __LINE__);
	} else {
		g_print("Open file success\n");
	}

	//if(write(fd, write_buf, strlen(write_buf)) != strlen(write_buf)){
	//	my_err("write", __LINE__);
	//}


	if(lseek(fdd, 0, SEEK_END) == -1){
	my_err("lseek", __LINE__);
	}
	if((len = lseek(fdd, 0, SEEK_CUR)) == -1){
		my_err("lseek", __LINE__);
	}
	if((lseek(fdd, 0, SEEK_SET)) == -1){
		my_err("lseek", __LINE__);
	}

	printf("len: %d\n", len);
	if((rett = read(fdd, contentt, len)) < 0){
		my_err("lseek",__LINE__);
	}
	strcpy(content, contentt);
	//for(i = 0; i< len; i ++){
	g_print("%s ", contentt);
	//}
	g_print("\n");


	g_print("/************************************************/");
	if(lseek(fdd, 10, SEEK_END) == -1){
		my_err("lseek", __LINE__);
	}
	
	
	//close (fdd);
/****************************************************************************************************/
 //FILE* srcFile = NULL;

	int srcLength =  lseek(fdd, 0, SEEK_CUR);
	int SNAPPY_OK = 0;
	char* pSrcBuffer =  (char*)g_malloc0(srcLength);
	read(fdd, pSrcBuffer, srcLength);
 	size_t cbOfCompressed = 32+ srcLength + srcLength/6;
  	char* pCompressedBuffer =  (char*)g_malloc0(cbOfCompressed); 
  	if (snappy_compress(pSrcBuffer, srcLength, pCompressedBuffer, &cbOfCompressed) == SNAPPY_OK)
  {
  	 free(pSrcBuffer);
   	pSrcBuffer = NULL;

   	if (snappy_validate_compressed_buffer(pCompressedBuffer, cbOfCompressed) == SNAPPY_OK)
   {
    	size_t cbDecompress = 0;
    	snappy_uncompressed_length(pCompressedBuffer, cbOfCompressed, &cbDecompress);

    	assert(cbDecompress == srcLength);

    	char* pDecompressedBuffer = (char*)g_malloc0(cbDecompress);
    	snappy_uncompress(pCompressedBuffer, cbOfCompressed, pDecompressedBuffer, (size_t*)&cbDecompress);

    	int file;
    	file = open("test1.txt", O_CREAT | O_RDWR );
    //_wfopen_s(&file, _T("123.pdf"), _T("ab"));
	write(file, pDecompressedBuffer, cbDecompress);
    	close(file);

    	free(pDecompressedBuffer);
    	pDecompressedBuffer = NULL;
   }
  }
 

	close (fdd);


/******************************************************************************************************************************************/
 //   while(offset < total_size){
 //       ret = hlfs_write(ctrl,content,request_size,offset);
  //      g_assert(ret==request_size);
  //      offset +=request_size;
  //      printf("offset:%d\n",offset);
  //  }
  //  g_print("TEST  hlfs write over \n");
//	g_print("test hlfs read\n");
//	sleep(2);
  // offset = 0;
   while(offset < len+1){
        ret = hlfs_read(ctrl,content,len,offset);
        g_assert(ret==len);
        offset +=len;
        printf("offset:%d\n",offset);
    }

	g_free(content);
	g_free(contentt);
	ret = hlfs_close(ctrl);
	deinit_hlfs(ctrl);
	g_print("TEST  hlfs test over \n");
	return 0;
}
#endif
