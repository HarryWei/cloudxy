/*
  *  Copyright (C) 2013 Harry Wei <harryxiyou@gmail.com>
  *  
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include <stdio.h>
#include <stdint.h>
#include <glib.h>
#include <string.h>
#include "hlfs_ctrl.h"
#include "dentry.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "misc.h"
#include "comm_define.h"

static int dentry2text(const struct dentry *dentry, 
						char *textbuf) 
{
	g_message("9999 enter func %s", __func__);
	const char *sep = HD_ITEM_SEP;
//	memset(textbuf, 0, strlen(textbuf));
	int n = sprintf(textbuf, "+%s%s%llu\n", 
					dentry->file_name, sep, 
					dentry->inode_no);
	g_message("9999 leave func %s", __func__);
	return n;
}

int dump_dentry(struct back_storage *storage, 
					const char *dentry_file, 
					struct dentry *dentry) {
	g_message("9999 enter func %s", __func__);
    if(dentry_file == NULL || storage == NULL || dentry == NULL) {
		g_message("Parameter error!");
        return -1;
    }
    int ret = 0;
    char dentry_text[1024];
    memset(dentry_text, 0, 1024);
    int len = dentry2text(dentry, dentry_text);
	g_message("dentry text is %s", dentry_text);
    ret = file_append_contents(storage, dentry_file, dentry_text ,len);
    return ret;
}

static int dentry_delmark2text(const char *dname, char *textbuf) {
	//HLOG_DEBUG("enter func %s", __func__);
	const char *sep = HD_ITEM_SEP;
	memset(textbuf, 0, 128);
	int n = sprintf(textbuf, "-%s%s\n", dname, sep);
	//HLOG_DEBUG("leave func %s", __func__);
	return n;
}

int dump_dentry_delmark(struct back_storage *storage, 
							const char *dentry_file, 
							const char *dname){
	//HLOG_DEBUG("enter func %s", __func__);
    if (dentry_file == NULL || dname == NULL || storage == NULL) {
		HLOG_ERROR("Parameter Error!");
        return -1;
    }
	int ret = 0;
    char dentry_delmark_text[128];
    memset(dentry_delmark_text, 0, 128);
	int len = dentry_delmark2text(dname, dentry_delmark_text);
	//HLOG_DEBUG("cp text is %s", snapshot_delmark_text);
    ret = file_append_contents(storage, dentry_file, dentry_delmark_text, len);
	return ret;
}

#if 0
/* load all snapshot will remove del snapshot and revise relation upname */
static void predicate_same_upname_snapshot(gpointer key,gpointer value,gpointer user_data){
       char * ss_name = (char*)key;
       struct snapshot *ss = (struct snapshot*)value;
       struct snapshot *del_ss = (struct snapshot *)user_data;
       if(g_strcmp0(ss->up_sname,del_ss->sname) == 0){
		   snprintf(ss->up_sname, HLFS_FILE_NAME_MAX, "%s", del_ss->up_sname);
       }
       return ;
}
#endif

static void revise_dentry_relation(GHashTable *ds_hashtable, GList *remove_list) {
     int i = 0;
     for(i = 0; i < g_list_length(remove_list); i++){
        char * ds_name = g_list_nth_data(remove_list, i);
	    struct dentry *ds = g_hash_table_lookup(ds_hashtable, ds_name);
        g_assert(ds!=NULL);
//        char *up_ss_name = ss->up_sname;
//        g_hash_table_foreach (ss_hashtable, predicate_same_upname_snapshot, ss);
        g_hash_table_remove(ds_hashtable, ds->file_name);
		g_free(ds);
     }
     return ;
}

static int load_dentry_from_text(struct dentry **ds, const char *buf, int *flag)
{
	g_message("9999 enter func %s", __func__);
	const char *sep = HD_ITEM_SEP;
    gchar **v = g_strsplit(buf, sep, 2);
    if ('+' == v[0][0]) {
        *flag = 0;
//		gchar **_v = g_strsplit(v[1], sep, 3);
    	gchar *_ds_name = v[0] + 1;
//    	gchar *_version = _v[0];
		gchar *_inode_no = v[1];
//    	gchar *_up_ss_name = _v[2];
    	char *endptr = NULL;
        (*ds) = (struct dentry *) g_malloc0(sizeof(struct dentry));
		if ((*ds) == NULL) {
			g_message("Allocate error!");
			return -1;
		}
//		(*ds)->timestamp = strtoull(_version, &endptr, 0); 
    	sprintf((*ds)->file_name, "%s", _ds_name);
//		sprintf((*ss)->up_sname, "%s", _up_ss_name);
		(*ds)->inode_no = strtoull(_inode_no, &endptr, 0); 
		g_message("%s's inode no is %lu", (*ds)->file_name, 
												(*ds)->inode_no);
//		g_strfreev(_v);
    } else if ('-' == v[0][0]) {
        *flag = 1;
    	gchar *_ds_name = v[0] + 1;
        (*ds) = (struct dentry*)g_malloc0(sizeof(struct dentry));
		if ((*ds) == NULL) {
			g_message("Allocate error!");
			return -1;
		}
    	sprintf((*ds)->file_name, "%s", _ds_name);
    } else {
		g_message("flag parse error");
		g_strfreev(v);
		return -1; 
	}   
	g_strfreev(v);
	g_message("9999 leave func %s", __func__);
	return 0;
}

int load_all_dentry(struct back_storage *storage,
					const char* dentry_file,
					GHashTable *ds_hashtable)
{
	g_message("999 enter func %s", __func__);
	int ret = 0;
    char *contents = NULL;
    uint32_t size;
    ret = file_get_contents(storage, dentry_file, &contents, &size);
    if(ret !=0){
		/*TODO: handle this condition*/
	   HLOG_ERROR("can not read dentry content, but may be first start, not error, check it please");
       return -1; 
    }
    gchar **lines = g_strsplit(contents, "\n", 0);

    g_message("g strv length:%d:", g_strv_length(lines));
    GList *to_remove_ds_list = NULL;
    int i = 0;
	for (i = 0; i < g_strv_length(lines) - 1; i++) {
		int flag = -1;
		struct dentry *ds = NULL;
		ret = load_dentry_from_text(&ds, lines[i], &flag);
        if(ret !=0){
            goto out;
        }
		if (flag == 0) {
            g_hash_table_insert(ds_hashtable, ds->file_name, ds);
            if(to_remove_ds_list != NULL) {
                int j = 0;
                for(j = 0; j < g_list_length(to_remove_ds_list); j++){
                    char * ds_name = g_list_nth_data(to_remove_ds_list,j);
                    g_message("ds_name [%s] ...", ds_name);
                    if(0 == strcmp(ds->file_name, ds_name)){
                        to_remove_ds_list = g_list_remove(to_remove_ds_list, ds_name);
                        break;
                    }
                }
            }
            g_message("insert dentry [%s]", ds->file_name);
		} else if (flag == 1) {
			g_message("remove dentry [%s]", ds->file_name);
            to_remove_ds_list = g_list_append(to_remove_ds_list, ds->file_name);
		} else {
			g_message("error - flag");
            g_assert(0);
            goto out;
		}
	}

	if (NULL != to_remove_ds_list) {
    	revise_dentry_relation(ds_hashtable,to_remove_ds_list);
	}

out:
	g_strfreev(lines);
	g_free(contents);
    if(to_remove_ds_list!=NULL) g_list_free(to_remove_ds_list);
	g_message("999 leave func %s", __func__);
	return ret;
}

#if 0
/*  dump snapshot by timestamp order  */
static int compare_snapshot(gconstpointer litem,
                        gconstpointer ritem){
        int ret = 0;
		struct snapshot *ls = (struct snapshot *)litem;
		struct snapshot *rs = (struct snapshot *)ritem;
        if(ls->timestamp < rs->timestamp){
          ret = -1;
        }else if(ls->timestamp > rs->timestamp){
          ret =  1;
        }else{
          ret =  0;
        }
     return ret;
}

int sort_all_snapshot(GHashTable *ss_hashtable,GList **ss_list){
    (*ss_list) = g_hash_table_get_values(ss_hashtable);
    (*ss_list) = g_list_sort((*ss_list),compare_snapshot);
    return 0;
}

#endif
#if 0
/*  order by timestamp */
int redump_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable){
 	//HLOG_DEBUG("enter func %s", __func__);
    if(storage == NULL || snapshot_file == NULL || ss_hashtable == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }
    int ret = 0;
    GList *ss_list = g_hash_table_get_values(ss_hashtable);
    ss_list = g_list_sort(ss_list,compare_snapshot);
    int i;
    char content[1024*1024];
    uint32_t pos=0;
    memset(content,0,1024*1024);
    for(i = 0; i < g_list_length(ss_list); i++){
        struct snapshot *ss = g_list_nth_data(ss_list,i);
        char snapshot_text[1024];
        memset(snapshot_text,0,1024);
	    int len = snapshot2text(ss,snapshot_text);
        memcpy(content+pos,snapshot_text,len);
        pos += len;
    }

	if((0 == storage->bs_file_is_exist(storage,snapshot_file)) && 
			(0 > storage->bs_file_delete(storage,snapshot_file))) {
		HLOG_ERROR("remove old snapshot file failed");
		return -1;
	}

    if(pos != file_append_contents(storage,snapshot_file,content,pos)){
		HLOG_ERROR("write snapshot error!");
        return -1;
    }
    return ret;
}
#endif

int load_dentry_by_name(struct back_storage *storage, 
						const char *dentry_file,
						struct dentry **ds,
						const char *dname)
{
	//HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	struct dentry *_ds = NULL;
	GHashTable *ds_hashtable = g_hash_table_new(g_str_hash, g_str_equal);
	ret = load_all_dentry(storage, dentry_file, ds_hashtable);
	if (ret < 0) {
		HLOG_ERROR("load all dentry error, but may be first start, not error, check it please");
		g_hash_table_destroy(ds_hashtable);
		return -1;
	}
	_ds = g_hash_table_lookup(ds_hashtable, dname);
    if(_ds == NULL){
	   HLOG_DEBUG("Can not find this dentry name");
       ret = 1;
	   goto out;
    }
	ret = 0;
    (*ds) = (struct dentry*)g_malloc0(sizeof(struct dentry));
	if (NULL == (*ds)) {
		HLOG_ERROR("Allocate error!");
		return -1;
	}
	(*ds)->inode_no = _ds->inode_no;	
    sprintf((*ds)->file_name, "%s", _ds->file_name);
out:
	g_hash_table_destroy(ds_hashtable);
	g_message("999 leave func %s", __func__);
	return ret;
}

#if 0
static int load_all_alive_snapshot(struct back_storage *storage,const char* alive_snapshot_file,GList **alive_ss_list)
{
	//HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
    char *contents = NULL;
    uint32_t size;
    ret = file_get_contents(storage,alive_snapshot_file,&contents,&size);
    if(ret != 0){
	   HLOG_ERROR("file get contents error!");
       return -1; 
    }
	gchar **lines = g_strsplit(contents, "\n", 0);
    g_free(contents);
    int i = 0;
	//HLOG_DEBUG("999 lines length is %d", g_strv_length(lines));
	for (i = 0; i < g_strv_length(lines) - 1; i++) {
        struct snapshot *ss = NULL;
        int flag=0;
        ret = load_snapshot_from_text(&ss,lines[i], &flag);
        if(ret!=0){
            return -1;
        }
        (*alive_ss_list) = g_list_append((*alive_ss_list),ss);
    }
	//HLOG_DEBUG("9999 leave func %s", __func__);
	return ret;
}


static void free_all_list(GList *list){
   int i;
   for(i =0;i< g_list_length(list);i++){
        gpointer data = g_list_nth_data(list,i);
        g_free(data);
   }
   g_list_free(list);
}

int find_latest_alive_snapshot_before_time(struct back_storage *storage,
										const char* snapshot_file,
										const char* alive_snapshot_file, 
										struct snapshot **ss,
										uint64_t timestamp){
    //HLOG_DEBUG("enter func %s", __func__);
    if(snapshot_file == NULL || alive_snapshot_file == NULL || storage == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }  
    int ret = 0;
	GHashTable *ss_hashtable = g_hash_table_new(g_str_hash, g_str_equal);
    GList * alive_snapshot_list = NULL;
    ret = load_all_alive_snapshot(storage,alive_snapshot_file,&alive_snapshot_list);
    if(ret !=0){
        goto out;
    }
	ret = load_all_snapshot(storage,snapshot_file,ss_hashtable);
	if (ret < 0) {
		g_hash_table_destroy(ss_hashtable);
		goto out;
    }
    gboolean flag = FALSE;
    int i;
	//HLOG_DEBUG("99 timestamp is %llu", timestamp);
	//HLOG_DEBUG("99 list length is %d", g_list_length(alive_snapshot_list));
	if (0 == g_list_length(alive_snapshot_list)) {
		HLOG_ERROR("alive snapshot list is nil");
		ret = -1;
		goto out;
	}
    for(i = g_list_length(alive_snapshot_list) - 1; i >= 0; i--){
        struct snapshot *_ss = g_list_nth_data(alive_snapshot_list,i);
		HLOG_DEBUG("99 i _ss->timestamp is %llu, _ss->sname is %s", _ss->timestamp, _ss->sname);
        if(_ss->timestamp <= timestamp){
           if(NULL != g_hash_table_lookup(ss_hashtable,_ss->sname)){
             (*ss) = (struct snapshot*)g_malloc0(sizeof(struct snapshot));
			 if ((*ss) == NULL) {
				 HLOG_ERROR("Allocate Error!");
				 ret = -1;
				 goto out;
			 }
             memcpy((*ss),_ss,sizeof(struct snapshot));
             flag = TRUE;
             break;
            }
        }
    }
	//HLOG_DEBUG("99 *ss addr %p", (*ss));
    if(flag==FALSE){
       ret = -1;
    }
out:
    if(alive_snapshot_list !=NULL){
       free_all_list(alive_snapshot_list);
    }
//    g_hash_table_destroy(ss_hashtable);
    //HLOG_DEBUG("9999 leave func %s", __func__);
    return ret;
}
#if 0
int is_first_start(struct back_storage *storage, 
				const char * snapshot_file, 
				const char *alive_snapshot_file) {
	if (EHLFS_NOFILE == storage->bs_file_is_exist(storage,snapshot_file) &&
			EHLFS_NOFILE == storage->bs_file_is_exist(storage,alive_snapshot_file)) {
		HLOG_DEBUG("first start hlfs ...");
		return HLFS_FS;
	}
	if (EHLFS_NOFILE != storage->bs_file_is_exist(storage,snapshot_file) &&
			EHLFS_NOFILE == storage->bs_file_is_exist(storage,alive_snapshot_file)) {
		HLOG_ERROR("Can not find alive snapshot file!");
		return EHLFS_UNKNOWN;
	}
	if (EHLFS_NOFILE == storage->bs_file_is_exist(storage,snapshot_file) &&
			EHLFS_NOFILE != storage->bs_file_is_exist(storage,alive_snapshot_file)) {
		HLOG_ERROR("Can not find snapshot file!");
		return EHLFS_UNKNOWN;
	}
	return 0;
}
#endif
int find_latest_alive_snapshot(struct back_storage *storage,
							const char* alive_snapshot_file, 
							const char* snapshot_file,
							struct snapshot **ss) {
    return find_latest_alive_snapshot_before_time(storage,alive_snapshot_file,snapshot_file,ss,G_MAXUINT64);
}

int dump_alive_snapshot(struct back_storage* storage,const char *alive_snapshot_file,struct snapshot *snapshot){
    return dump_snapshot(storage,alive_snapshot_file,snapshot);
}

#endif
