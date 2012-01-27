
 /*  kanghua <kanghua151@gmail.com> (C) 2011 */
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include "hlfs_ctrl.h"
#include "storage_helper.h"
#include "hlfs_log.h"
#include "snapshot.h"
#include "misc.h"
#include "comm_define.h"


static int snapshot2text(const struct snapshot *snapshot, char *textbuf) 
{
	HLOG_DEBUG("enter func %s", __func__);
	memset(textbuf, 0, strlen(textbuf));
	int n = sprintf(textbuf, "+%sSS_ITEM_SEP%lluSS_ITEM_SEP%lluSS_ITEM_SEP%s\n", 
					snapshot->sname,snapshot->timestamp, 
					snapshot->inode_addr, snapshot->up_sname);
	HLOG_DEBUG("leave func %s", __func__);
	return n;
}

int dump_snapshot(struct back_storage *storage, 
					const char *snapshot_file, 
					struct snapshot *snapshot) {
	HLOG_DEBUG("enter func %s", __func__);
    if(snapshot_file == NULL || snapshot == NULL || storage == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }
    int ret = 0;
    char snapshot_text[1024];
    memset(snapshot_text, 0, 1024);
	int len = snapshot2text(snapshot, snapshot_text);
	HLOG_DEBUG("ss text is %s", snapshot_text);
    ret = file_append_contents(storage,snapshot_file,snapshot_text,len);
    return ret;
}

static int snapshot_delmark2text(const char *ssname, char *textbuf) {
	HLOG_DEBUG("enter func %s", __func__);
	memset(textbuf, 0, 128);
	int n = sprintf(textbuf, "-%sSS_ITEM_SEP\n", ssname);
	HLOG_DEBUG("leave func %s", __func__);
	return n;
}

int dump_snapshot_delmark(struct back_storage *storage, 
							const char *snapshot_file, 
							const char *ssname){
	HLOG_DEBUG("enter func %s", __func__);
    if (snapshot_file == NULL || ssname == NULL || storage == NULL) {
		HLOG_ERROR("Parameter Error!");
        return -1;
    }
	int ret = 0;
    char snapshot_delmark_text[1024];
    memset(snapshot_delmark_text, 0, 1024);
	int len = snapshot_delmark2text(ssname, snapshot_delmark_text);
	HLOG_DEBUG("cp text is %s", snapshot_delmark_text);
    ret = file_append_contents(storage,snapshot_file,snapshot_delmark_text,len);
}

/* load all snapshot will remove del snapshot and revise relation upname */
static gboolean predicate_same_upname_snapshot(gpointer key,gpointer value,gpointer user_data){
       char * ss_name = (char*)key;
       struct snapshot *ss = (struct snapshot*)value;
       char * del_ss_name = (char*)user_data;
       if(g_strcmp0(ss_name,del_ss_name) == 0){
          return TRUE; 
       }
       return FALSE;
}

static void revise_snapshot_relation(GHashTable *ss_hashtable,GList *remove_list){
     int i;
     for(i = 0; i < g_list_length(remove_list); i++){
        char * ss_name = g_list_nth_data(remove_list,i);
	    struct snapshot *ss = g_hash_table_lookup(ss_hashtable,ss_name);
        g_assert(ss!=NULL);
        char *up_ss_name = ss->up_sname;
        struct snapshot *revise_ss = g_hash_table_find (ss_hashtable,predicate_same_upname_snapshot,ss_name);
        if(revise_ss !=NULL){
		   snprintf(revise_ss->up_sname,HLFS_FILE_NAME_MAX,"%s",ss->up_sname);
        }
        g_hash_table_remove(ss_hashtable, ss->sname);
        g_free(ss);
     }
     return ;
}

static int load_snapshot_from_text(struct snapshot **ss, const char *buf, int *flag)
{
	HLOG_DEBUG("enter func %s", __func__);
    gchar **v = g_strsplit(buf, "SS_ITEM_SEP", 2);
    if ('+' == v[0][0]) {
        *flag = 0;
		gchar **_v = g_strsplit(v[1], "SS_ITEM_SEP", 3);
    	gchar *_ss_name = v[0] + 1;
    	gchar *_version = _v[0];
		gchar *_ime_inode_addr = _v[1];
    	gchar *_up_ss_name = _v[2];
    	char *endptr = NULL;
        *ss = (struct snapshot*)g_malloc0(sizeof(struct snapshot));
		(*ss)->timestamp = strtoull(_version, &endptr, 0); 
    	sprintf((*ss)->sname, "%s", _ss_name);
		sprintf((*ss)->up_sname, "%s", _up_ss_name);
		(*ss)->inode_addr = strtoull(_ime_inode_addr, &endptr, 0); 
		HLOG_DEBUG("ss->timestamp:%llu  ss->sname:%s  ss->up_sname:%s, \
					ss->inode_addr: %llu", (*ss)->timestamp, (*ss)->sname, 
					(*ss)->up_sname, (*ss)->inode_addr);
		g_strfreev(_v);
    } else if ('-' == v[0][0]) {
        *flag = 1;
    	gchar *_ss_name = v[0] + 1;
    	sprintf((*ss)->sname, "%s", _ss_name);
    } else {
		HLOG_ERROR("flag parse error");
		g_strfreev(v);
		return -1; 
	}   
	g_strfreev(v);
	HLOG_DEBUG("leave func %s", __func__);
	return 0;
}


int load_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
    char *contents = NULL;
    uint32_t size;
    ret = file_get_contents(storage,snapshot_file,&contents,&size);
    if(ret !=0){
	   HLOG_ERROR("can not read snapshot content!");
       return -1; 
    }
	gchar **lines = g_strsplit(contents, "\n", 0);
    g_free(contents);
	HLOG_DEBUG("g strv length:%d:", g_strv_length(lines));
    GList *to_remove_ss_list = NULL;
    int i;
	for (i = 0; i < g_strv_length(lines) - 1; i++) {
		int flag = -1;
		struct snapshot *ss = NULL;
		ret = load_snapshot_from_text(&ss, lines[i], &flag);
        if(ret !=0){
            goto out;
        }
		if (flag == 0) {
			g_hash_table_insert(ss_hashtable, ss->sname, ss);
			HLOG_DEBUG("insert snapshot [%s]", ss->sname);
		} else if (flag == 1) {
			HLOG_DEBUG("remove snapshot [%s]", ss->sname);
            to_remove_ss_list = g_list_append(to_remove_ss_list,ss->sname);
		} else {
			HLOG_ERROR("error - flag");
            g_assert(0);
            goto out;
		}
	}
    
    revise_snapshot_relation(ss_hashtable,to_remove_ss_list);
    
out:
	g_strfreev(lines);
    if(to_remove_ss_list!=NULL) g_list_free(to_remove_ss_list);
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}

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

int sort_all_snapshot(GHashTable *ss_hashtable,GList *snapshot_list){
    GList *ss_list = g_hash_table_get_values(ss_hashtable);
    ss_list = g_list_sort(ss_list,compare_snapshot);
    return 0;
}


/*  order by timestamp */
int redump_all_snapshot(struct back_storage *storage,const char* snapshot_file,GHashTable *ss_hashtable){
 	HLOG_DEBUG("enter func %s", __func__);
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


int load_snapshot_by_name(struct back_storage *storage, const char* snapshot_file,struct snapshot **ss, const char *ss_name)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
	struct snapshot *_ss = NULL;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal,g_free,g_free);
	ret = load_all_snapshot(storage,snapshot_file,ss_hashtable);
	if (ret < 0) {
		HLOG_ERROR("load all ss error");
		g_hash_table_destroy(ss_hashtable);
		return -1;
	}
	_ss = g_hash_table_lookup(ss_hashtable, ss_name);
    if(_ss!=NULL){
       memcpy(ss,_ss,sizeof(struct snapshot));
       ret = 0;
    }
    (*ss) = (struct snapshot*)g_malloc0(sizeof(struct snapshot)); 
	(*ss)->timestamp = _ss->timestamp;	
    sprintf((*ss)->sname, "%s", _ss->sname);
	sprintf((*ss)->up_sname, "%s", _ss->up_sname);
	(*ss)->inode_addr = _ss->inode_addr;
	g_hash_table_destroy(ss_hashtable);
	HLOG_DEBUG("leave func %s", __func__);
	return ret;
}


static int load_all_alive_snapshot(struct back_storage *storage,const char* alive_snapshot_file,GList *alive_ss_list)
{
	HLOG_DEBUG("enter func %s", __func__);
	int ret = 0;
    char *contents = NULL;
    uint32_t size;
    ret = file_get_contents(storage,alive_snapshot_file,&contents,&size);
    if(ret !=0){
	   HLOG_ERROR("can not read snapshot content!");
       return -1; 
    }
	gchar **lines = g_strsplit(contents, "\n", 0);
    g_free(contents);
    int i;
	for (i = 0; i < g_strv_length(lines) - 1; i++) {
        struct snapshot *ss;
        int flag=0;
        ret = load_snapshot_from_text(&ss,lines[i], &flag);
        if(ret!=0){
            return -1;
        }
        alive_ss_list = g_list_append(alive_ss_list,ss);
    }
}


static void free_all_list(GList *list){
   int i;
   for(i =0;i< g_list_length(list);i++){
        gpointer data = g_list_nth_data(list,i);
        g_free(data);
   }
   g_list_free(list);
}

int find_latest_alive_snapshot_before_time(struct back_storage *storage,const char* alive_snapshot_file, const char* snapshot_file,struct snapshot **ss,uint64_t timestamp){
    HLOG_DEBUG("enter func %s", __func__);
    if(snapshot_file == NULL || alive_snapshot_file == NULL || storage == NULL) {
		HLOG_ERROR("Parameter error!");
        return -1;
    }  
    int ret = 0;
	GHashTable *ss_hashtable = g_hash_table_new_full(g_str_hash, g_str_equal,g_free,g_free);
    GList * alive_snapshot_list = NULL;
    ret = load_all_alive_snapshot(storage,alive_snapshot_file,alive_snapshot_list);
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
    for(i = g_list_length(alive_snapshot_list);i=0;i--){
        struct snapshot *_ss = g_list_nth_data(alive_snapshot_list,i);
        if(_ss->timestamp <= timestamp){
           if(NULL != g_hash_table_lookup(ss_hashtable,_ss->sname)){
             *ss = (struct snapshot*)g_malloc0(sizeof(struct snapshot));
             memcpy(*ss,_ss,sizeof(struct snapshot));
             flag = TRUE;
             break;
            }
        }
    }
    if(flag==FALSE){
       ret = -1;
    }
out:
    if(alive_snapshot_list !=NULL){
       free_all_list(alive_snapshot_list);
    }
    g_hash_table_destroy(ss_hashtable);
    return ret;
}

int find_latest_alive_snapshot(struct back_storage *storage,const char* alive_snapshot_file, const char* snapshot_file,struct snapshot **ss){
    return find_latest_alive_snapshot_before_time(storage,alive_snapshot_file,snapshot_file,ss,G_MAXUINT64);
}

int dump_alive_snapshot(struct back_storage* storage,const char *alive_snapshot_file,struct snapshot *snapshot){
    return dump_snapshot(storage,alive_snapshot_file,snapshot);
}
