#if 1
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include "glib.h"
#include "storage.h"
#include "hdfs.h"
#include "misc.h"
#include "hlfs_log.h"

/* FIX IT */
static gchar * build_hdfs_path(const char *uri,const char *path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
#if 0
     gchar **v  = g_strsplit(uri,"://",2);
     if (0!=g_strcmp0(v[0],"hdfs")){
        g_strfreev(v);
        return NULL;
     }
     gchar *full_path;
     if(v[1][0] != '/'){
        gchar **_v = g_strsplit(v[1],"/",2);  
        full_path = g_build_filename("/",_v[1],fs_name,path,NULL);
        //g_strfreev(v);
     }else{
        full_path = g_build_filename(v[1],fs_name,path,NULL);
     }
     //g_strfreev(v);
     return full_path;
#endif 
     char *head=NULL;
     char *hostname=NULL;
     char *dir=NULL;
     char *fs_name = NULL;
     int port;
     int ret = parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
     if(ret !=0){
	     HLOG_ERROR("parse_from_uri error!");
         return NULL;
     }
     gchar *full_path = g_build_filename(dir,fs_name,path,NULL);
     HLOG_DEBUG("full path is %s",full_path);
     g_free(head);
     g_free(hostname);
     //g_free(dir);
	 HLOG_DEBUG("hdfs -- leave func %s", __func__);
     return full_path;
}

int hdfs_connect(struct back_storage *storage,const char* uri){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    char *head=NULL;
    char *hostname=NULL;
    char *dir=NULL;
    char *fs_name = NULL;
    int port;
    int ret = parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
    if(ret !=0){
	     HLOG_ERROR("parse_from_uri error!");
        return -1;
    }
    //hdfsFS fs = hdfsConnect("default",0); // for local test
    hdfsFS fs = hdfsConnect(hostname,port); // for local test
    g_free(head);
    g_free(hostname);
    g_free(dir);
    if(NULL==fs){
	     HLOG_ERROR("fs is null, hdfsConnect error!");
        return -1;
    }
    storage->uri = uri; 
    storage->user = g_strdup("kanghua");
    storage->port = port;
    storage->fs_handler = (bs_fs_t)fs;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return 0;
}

int hdfs_disconnect(struct back_storage * storage){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    hdfsFS fs = (hdfsFS)storage->fs_handler;
    hdfsDisconnect(fs);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return 0;
}

int hdfs_file_close(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    hdfsFile hfile = (hdfsFile)file;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return hdfsCloseFile((hdfsFS)storage->fs_handler,hfile);
}

int hdfs_file_is_exist(struct back_storage * storage,const char *path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,path);
    HLOG_DEBUG("hdfs full path %s",full_path);

    if(0!=hdfsExists((hdfsFS)storage->fs_handler,full_path)){
        g_free(full_path);
	HLOG_ERROR("hdfsExists error");
        return -1;
    }
    g_free(full_path);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return 0;
}

uint64_t hdfs_file_tell(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return 0;
}

int hdfs_file_append(struct back_storage * storage,bs_file_t file,const char *write_buff,uint32_t write_len){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    hdfsFile hfile = (hdfsFile)file;
    HLOG_DEBUG("append hdfs file:%p,write_len:%d",hfile,write_len);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return hdfsWrite((hdfsFS)storage->fs_handler,hfile,write_buff,write_len);
}

int hdfs_file_pread(struct back_storage *storage,bs_file_t file,const char*read_buff,uint32_t read_len,uint64_t pos){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    hdfsFile hfile = (hdfsFile)file;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return hdfsPread((hdfsFS)storage->fs_handler,hfile,pos,(void *) read_buff,read_len);
}

int hdfs_file_flush(struct back_storage *storage,bs_file_t file){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    hdfsFile hfile = (hdfsFile)file;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return hdfsHFlush((hdfsFS)storage->fs_handler,hfile);
}

int hdfs_file_delete(struct back_storage *storage,const char* path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,path);
    int ret = hdfsDelete((hdfsFS)storage->fs_handler,full_path);
    g_free(full_path);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return ret;
}

bs_file_info_t * 
hdfs_file_info(struct back_storage *storage,const char* path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,path);
    hdfsFileInfo *hinfo = hdfsGetPathInfo((hdfsFS)storage->fs_handler,full_path);
    if(NULL == hinfo){
	    HLOG_ERROR("hdfsGetPathInfo error");
       return NULL;
    }
    bs_file_info_t *info = (bs_file_info_t*)g_malloc0(sizeof(bs_file_info_t));
    if (NULL == info) {
	    HLOG_ERROR("Allocate Error!");
	    return NULL;
    }
    strcpy((char *) info->name, (const char *) hinfo->mName);
    info->is_dir = 0;//?
    info->size = hinfo->mSize;
    info->lmtime = hinfo->mLastMod;
    free(hinfo);
    g_free(full_path);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return info;
}


bs_file_info_t*
hdfs_list_dir(struct back_storage * storage,const char * dir_path,uint32_t* num_entries){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,dir_path);
    int num;
    hdfsFileInfo *hinfos  = hdfsListDirectory((hdfsFS)storage->fs_handler,full_path,&num);
    if(NULL == hinfos){
	    HLOG_ERROR("hdfsListDirectory error");
       return NULL; 
    }
    hdfsFileInfo *hinfo = hinfos;
    bs_file_info_t *infos = (bs_file_info_t*)g_malloc0(sizeof(bs_file_info_t)*4096);
    if (NULL == infos) {
	    HLOG_ERROR("Allocate Error!");
	    return NULL;
    }
    bs_file_info_t *info = infos;
    int i;
    for(i=0;i<num;i++){
        strcpy((char *) info->name, (const char *) g_basename(hinfo->mName));
        info->is_dir = 0;//?
        info->size =   hinfo->mSize;
        info->lmtime = hinfo->mLastMod;
        info++;
        hinfo++;
    }
    free(hinfos);
    g_free(full_path);
    *num_entries = num;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return infos;
}

int hdfs_file_mkdir(struct back_storage * storage,const char *dir_path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,dir_path);
    HLOG_DEBUG("hdfs mkdir:%s",full_path);
    if(0==hdfsExists((hdfsFS)storage->fs_handler,full_path)){
        g_free(full_path);
        return -1;
    }
    if(0 != hdfsCreateDirectory((hdfsFS)storage->fs_handler,full_path)){
	HLOG_DEBUG("hdfsCreateDirectory error");
       g_free(full_path);
       return -1;
    }
    g_free(full_path);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return 0;
}

static bs_file_t  __hlfs_file_open(struct back_storage *storage,const char*path,int flags){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    gchar * full_path = build_hdfs_path(storage->uri,path);
    HLOG_DEBUG("hdfs full path %s",full_path);
    hdfsFile file = NULL;
    file =  hdfsOpenFile((hdfsFS)storage->fs_handler,full_path,flags, 0, 0, 0);
    if(NULL == file){
	HLOG_DEBUG("hdfsOpenFile error");
       return NULL;
    }
    HLOG_DEBUG("hdfs file:%p",file);
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return (bs_file_t)file;
}

bs_file_t  hdfs_file_create(struct back_storage *storage,const char *path){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    int _flags = O_WRONLY;
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return __hlfs_file_open(storage,path,_flags);
}

bs_file_t hdfs_file_open(struct back_storage *storage,const char *path,int flags){
	HLOG_DEBUG("hdfs -- enter func %s", __func__);
    int  _flags = 0;
    if(flags == BS_READONLY){
       _flags = O_RDONLY;
    }else if (flags == BS_WRITEABLE){
       _flags = O_WRONLY|O_APPEND;
    }else{
       HLOG_DEBUG("open with error flags:%d",flags);
    }
	HLOG_DEBUG("hdfs -- leave func %s", __func__);
    return  __hlfs_file_open(storage,path,_flags);
}

#endif

