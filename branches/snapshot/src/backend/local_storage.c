#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include "glib.h"
#include "storage.h"
#include "misc.h"
#include "hlfs_log.h"
#include "comm_define.h"

static gchar *build_local_path(const char *uri,const char *path){
	HLOG_DEBUG("local -- enter func %s", __func__);
     char *head=NULL;
     char *hostname=NULL;
     char *dir=NULL;
     char *fs_name = NULL;
     int port;
     int ret = parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
     if(ret !=0){
	     HLOG_ERROR("parse_from_uri error");
         return NULL;
     }
     gchar *full_path = g_build_filename(dir,fs_name,path,NULL);
     HLOG_DEBUG("full path is %s",full_path);
     g_free(head);
     g_free(hostname);
     g_free(dir);
	HLOG_DEBUG("local -- leave func %s", __func__);
     return full_path;
}


int local_connect(struct back_storage *storage,const char* uri){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //gchar **v  = g_strsplit(uri,"://",2);
    //if (0!=g_strcmp0(v[0],"local")){
    //   g_strfreev(v);
    //   return NULL;   
    //}
    //gchar * fs_path = g_build_filename(v[1],fs_name,NULL);
    //g_strfreev(v);
    //g_free(fs_path);
    //storage->fs_handler = fs_path;
    storage->uri = uri; 
    //storage->fs_name  = g_basename(uri);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return 0;
}

int local_disconnect(struct back_storage * storage){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //g_free((gchar*)storage->fs_handler);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return 0;
}

bs_file_t local_file_open(struct back_storage *storage,const char *path,int flags){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return NULL;
    }
    HLOG_DEBUG("full path %s", full_path);
    int fd=0;
    if(flags == BS_READONLY){
       fd = open(full_path,O_RDONLY);
    }else if (flags == BS_WRITEABLE){
       fd = open(full_path,O_APPEND|O_WRONLY,00700);
    }else{
       HLOG_DEBUG("open with error flags:%d",flags);
    }
    if(fd == -1){
	    HLOG_ERROR("open file error");
       g_free(full_path);
       return NULL;
    }
#if 0
    bs_file_t file = (bs_file_t)g_malloc0(sizeof(int));
    if (NULL == file) {
	    g_message("%s - Allocate Error!\n", __func__);
	    g_free(full_path);
	    return NULL;
    }
    *(int*)file = fd;
#endif 
    bs_file_t file = (bs_file_t)GINT_TO_POINTER(fd); 
    g_free(full_path);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return file;
}

int local_file_close(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //int fd = *(int*)file;
    int fd = GPOINTER_TO_INT((gpointer)file);
//    close(fd);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return close(fd);
}

int local_file_is_exist(struct back_storage * storage,const char *path){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return EHLFS_MEM;
    }
    HLOG_DEBUG("full path %s",full_path);
    if (!g_file_test(full_path,G_FILE_TEST_EXISTS)){
        g_free(full_path);
        return EHLFS_NOFILE;
    }
    g_free(full_path);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return 0;
}

uint64_t local_file_tell(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("local -- enter func %s", __func__);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return 0;
}

int local_file_append(struct back_storage * storage,bs_file_t file,const char *write_buff,uint32_t write_len){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //int fd = *(int*)file;
    int fd = GPOINTER_TO_INT((gpointer)file);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return write(fd,write_buff,write_len);
}

int local_file_pread(struct back_storage *storage,bs_file_t file,const char*read_buff,uint32_t read_len,uint64_t pos){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //int fd = *(int*)file;
    int fd = GPOINTER_TO_INT((gpointer)file);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return pread(fd,(void*)read_buff,read_len,pos);
}

int local_file_flush(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("local -- enter func %s", __func__);
    //int fd = *(int*)file;
    int fd = GPOINTER_TO_INT((gpointer)file);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return fsync(fd);
}

int local_file_delete(struct back_storage *storage,const char* path){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return -1;
    }
    int ret = remove(full_path);
    g_free(full_path);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return ret;
}

bs_file_info_t * 
local_file_info(struct back_storage *storage,const char* path){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return NULL;
    }
    struct stat buf;
    int res=lstat(full_path,&buf);
    bs_file_info_t *info = NULL;
    if(res != 0){
	    HLOG_ERROR("lstat error");
       g_free(full_path);
       return NULL;
    }
    info = (bs_file_info_t*)g_malloc0(sizeof(bs_file_info_t));
    strcpy((char *) info->name,path);
    info->is_dir = g_file_test (full_path, G_FILE_TEST_IS_DIR);
    info->size = buf.st_size;
    info->lmtime = buf.st_mtime;
    g_free(full_path);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return info;
}


bs_file_info_t*
local_list_dir(struct back_storage * storage,const char * dir_path,uint32_t* num_entries){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,dir_path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return NULL;
    }
    GDir * dir = g_dir_open(full_path,0,NULL);
    if(dir==NULL){
	    HLOG_ERROR("g_dir_open error");
        g_free(full_path);
        return NULL;
    }
    struct stat buf;
    bs_file_info_t * infos = (bs_file_info_t*)g_malloc0(sizeof(bs_file_info_t)*4096);
    if (NULL == infos) {
	    g_free(full_path);
	    HLOG_ERROR("build local path error!");
	    return NULL;
    }
    const gchar *filename;
    bs_file_info_t *info = infos;
    int idx=0;
    while((filename = g_dir_read_name(dir)) != NULL) {
        gchar *file_path = g_build_filename(full_path,filename,NULL);
        int res=lstat(file_path,&buf);
	if (0 != res) {
		g_free(full_path);
		g_free(file_path);
		g_dir_close(dir);
		HLOG_ERROR("lstat error");
		return NULL;
	}
        strcpy((char *) info->name,filename);
        info->is_dir = g_file_test(file_path, G_FILE_TEST_IS_DIR);
        info->size = buf.st_size;
        info->lmtime = buf.st_mtime;
        info++;
        idx++;
        g_free(file_path);
    }
    *num_entries = idx;
    g_free(full_path);
    g_dir_close(dir);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return infos;
}

int local_file_mkdir(struct back_storage * storage,const char *dir_path){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,dir_path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return -1;
    }
    HLOG_DEBUG("full path is %s",full_path);
    if(0!=g_mkdir(full_path,00700)){
	    HLOG_ERROR("g_mkdir error");
       g_free(full_path);
       return -1;
    }
    g_free(full_path);
	HLOG_DEBUG("local -- leave func %s", __func__);
    return 0;
}

bs_file_t local_file_create(struct back_storage *storage,const char *path){
	HLOG_DEBUG("local -- enter func %s", __func__);
    gchar * full_path = build_local_path(storage->uri,path);
    if (NULL == full_path) {
	    HLOG_ERROR("build local path error!");
	    return NULL;
    }
    HLOG_DEBUG("full path:%s", full_path);
    int fd = g_creat(full_path,00700);
    g_free(full_path);
#if 0
    bs_file_t file = (bs_file_t)g_malloc0(sizeof(int));
    if (NULL == file) {
	    g_message("%s - build local path error!\n", __func__);
	    g_free(full_path);
	    return NULL;
    }
    *(int*)file = fd;
#endif 
    bs_file_t file = (bs_file_t)GINT_TO_POINTER(fd); 
	HLOG_DEBUG("local -- leave func %s", __func__);
    return file;
}
