/*
  *  Copyright (C) 2012      KangHua <kanghua151@gmail.com> 
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <stdlib.h>
#include "address.h"
#include "storage.h"
#include "bs_local.h"
#include "hlfs_log.h"
#include "comm_define.h"

uint64_t get_current_time(void)
{
	//HLOG_DEBUG("enter func %s", __func__);
	struct timeval t;

	if (gettimeofday(&t, NULL)) {
		HLOG_ERROR("Get current time error!");
		return -1;
	}
	//HLOG_DEBUG("leave func %s", __func__);
	return (uint64_t) t.tv_sec * 1000 + t.tv_usec / 1000;	
}
#if 0
char *build_segfile_name(uint64_t segno)
{
    	char s[64];
    	sprintf(s, "%llu", segno);
    	gchar * segfile_name = g_strconcat(s, ".", "seg", NULL);
    	return segfile_name;
}

char *build_segfile_name_by_address(uint64_t storage_address)
{
	uint32_t segno  = get_segno(storage_address);
    	char s[64];
    	sprintf(s, "%d", segno);
    	gchar * segfile_name = g_strconcat(s,".","seg",NULL);
    	return segfile_name;
}
#endif 
int  build_segfile_name(uint32_t segno, const char *segfile_name)
{
    //HLOG_DEBUG("enter func %s", __func__);
    if(segfile_name==NULL){
        HLOG_ERROR("segfile_name is null");
        return -1;
    }
    snprintf((char *) segfile_name,SEGMENT_FILE_NAME_MAX,"%u%s%s",segno,".","seg");
    //HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

int  build_segfile_name_by_address(uint64_t storage_address, const char*segfile_name)
{
    //HLOG_DEBUG("enter func %s", __func__);
    if(segfile_name==NULL){
        return -1;
    }
    uint32_t segno  = get_segno(storage_address);
    snprintf((char *) segfile_name,SEGMENT_FILE_NAME_MAX,"%u%s%s",segno,".","seg");
    //HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

uint32_t get_segfile_no(const char * segfile)
{
    //HLOG_DEBUG("enter func %s", __func__);
    const gchar *basename = g_basename(segfile);
    gchar **v = g_strsplit(basename,".",2);
    uint32_t segno = atol(v[0]);
    g_strfreev(v);
    //HLOG_DEBUG("leave func %s", __func__);
    return segno;
}


int read_block(struct back_storage *storage ,uint64_t storage_address,uint32_t block_size,char *block_buf)
{
    //HLOG_DEBUG("enter func %s", __func__);
	if(NULL == storage || NULL == block_buf){
		return -1;
	}
    int ret = 0;
    uint32_t offset = get_offset(storage_address); 
    const char segfile_name[SEGMENT_FILE_NAME_MAX];
    ret = build_segfile_name(get_segno(storage_address), (char *) segfile_name);
    if (-1 == ret) {
        HLOG_ERROR("build_segfile_name error");
        ret = -1;
		goto out;
    }

    bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY); 
    if(file==NULL){
        HLOG_ERROR("can not open segment file %s",segfile_name);
        ret = -1;
		goto out;
    }
	
    //char * block = (char*)g_malloc0(block_size);
    //if (NULL == block) {
    //    HLOG_ERROR("Allocate Error!");
    //    block = NULL;
	//    goto out;
    //}
    uint32_t read_size;
    if(block_size != (read_size = storage->bs_file_pread(storage,file,block_buf,block_size,offset))){
	    HLOG_ERROR("bs_file_pread's size:%d is not equal to block_size:%d, at offset:%u",read_size,block_size,offset);
        ret = -1;
        goto out;
    }

out:
    storage->bs_file_close(storage,file);
	//HLOG_DEBUG("leave func %s", __func__);
    return ret;
}

/* 
 * uri format --
 * local:///tmp/testenv/testfs
 * hdfs:///tmp/testenv/testfs
 * hdfs://localhost:8020/tmp/testenv/testfs
 * hdfs://localhost/tmp/testenv/testfs
 * hdfs://192.168.0.1:8020/tmp/testenv/testfs
 * */
int parse_from_uri(const char *uri, char ** head, char** hostname ,char** dir,char** fs_name,int* port)
{
	//HLOG_DEBUG("enter func %s", __func__);
    gchar **v=NULL;
    gchar **v1=NULL;
    gchar **v2=NULL;
    char *pre_uri = g_dirname(uri);
    *fs_name = g_strdup((char *) g_basename (uri));
    v = g_strsplit(pre_uri,"://",2);
	g_free(pre_uri); 
    if(g_strv_length(v)!=2){
       g_strfreev(v);
       return -1; 
    }
    *head = g_strdup(v[0]);
        
    if( v[1][0] == '/' ){
        //HLOG_DEBUG("default localhost for hostname\n");
        *hostname = g_strdup("default");
        *dir = g_strdup(v[1]);
        *port = 0; 
    }else{
        v1 = g_strsplit(v[1],"/",2);
        if(g_strv_length(v1) < 2){
           g_strfreev(v);
           g_strfreev(v1);
           return -1;
        }
        v2 = g_strsplit(v1[0],":",2);
        if(g_strv_length(v2)!=2){
           *hostname = g_strdup(v1[0]);
		   char _dir[128];
		   memset(_dir, 0, 128);
		   sprintf(_dir, "%s%s", "/", v1[1]);
           *dir = g_strdup(_dir);
           *port = 8020;
        }else{
           *hostname = g_strdup(v2[0]);
		   char _dir[128];
		   memset(_dir, 0, 128);
		   sprintf(_dir, "%s%s", "/", v1[1]);
           *dir = g_strdup(_dir);
           *port = atoi(v2[1]);
        }
    }
    g_strfreev(v);
    g_strfreev(v1);
    g_strfreev(v2);
	//HLOG_DEBUG("leave func %s", __func__);
    return 0;
}
