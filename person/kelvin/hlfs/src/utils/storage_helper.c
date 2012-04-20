#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <signal.h>
#include "glib.h"
#include "comm_define.h"
#include "hlfs_ctrl.h"
#include "hlfs_log.h"
#include "storage.h"
#include "address.h"
#include "misc.h"
#include "bs_local.h"
#include "bs_hdfs.h"

//struct back_storage* init_storage_handler(const char* uri,const char *fs_name)
struct back_storage* init_storage_handler(const char* uri)
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret =0;
    struct back_storage* storage = NULL;
    gchar* back_storage_type;
    char *head=NULL;
    char *hostname=NULL;
    char *dir=NULL;
    char *fs_name = NULL;
    int port;
    ret = parse_from_uri(uri,&head,&hostname,&dir,&fs_name,&port);
    if(ret !=0){
	    HLOG_ERROR("parse_from_uri happened error");
        return NULL;
    }
    gchar *fs_path = g_build_filename(dir,fs_name,NULL);
    HLOG_DEBUG("loc [fs:%s], [path:%s]\n",fs_name,fs_path);
    if (0==g_strcmp0(head,"local")){
        storage=get_local_storage_ops();
    }else if(0 == g_strcmp0(head,"hdfs")){
        storage = get_hdfs_storage_ops();
    }else{
        HLOG_ERROR("Error URI");
        ret = -1;
        goto out;
    }

    if(0!=storage->bs_fs_connect(storage,uri)){
        ret = -1;
        goto out;
        HLOG_ERROR("connect filesystem failed");
        return NULL;
    }
out:
    g_free(fs_path);
    g_free(head);
    g_free(hostname);
    g_free(dir);
    if(ret !=0){
        free(storage);
	HLOG_ERROR("ret is not 0, so error happened");
       storage = NULL;
    }
	HLOG_DEBUG("leave func %s", __func__);
    return storage;
}


int deinit_storage_handler(struct back_storage * storage){
	HLOG_DEBUG("enter func %s", __func__);
    if(storage == NULL){
       HLOG_ERROR("storage is NULL");
       return -1;
    }
    if(0!=storage->bs_fs_disconnect(storage)){
       return -1;
    }
    HLOG_DEBUG("disconnect succ");
    //g_free(storage->storage_name);
    //g_message("%s disconnect succ\n",__func__);
    //g_free(storage->fs_name);
    //g_free(storage->fs_location);
    //g_message("%s disconnect succ\n",__func__);
    //g_free(storage->user);
	HLOG_DEBUG("leave func %s", __func__);
    return 0;
}

/*
 * get_last_inode_addr: get the segment's lastest log's inode address.
 * @para ctrl: the global control structure of HLFS.
 * @para segfile: the segment file name.
 * @return: if error happens return -1, if right return the inode address.
 */
uint64_t get_last_inode_storage_addr_in_seg( struct back_storage * storage, uint32_t segno) 
{
    HLOG_DEBUG("enter func %s",__func__);
#if 1
	bs_file_t file = NULL;
	bs_file_info_t *info = NULL;
	struct inode_map_entry im;
    const char segfile[SEGMENT_FILE_NAME_MAX];
    build_segfile_name(segno,segfile);
	if (0 == storage->bs_file_is_exist(storage, segfile)) {
		if (NULL == (file = storage->bs_file_open(storage, segfile, BS_READONLY))) {
			HLOG_ERROR("open segfile error!");
			return -1;
		}
		if (NULL == (info = storage->bs_file_info(storage, segfile))) {
			storage->bs_file_close(storage, file);
			HLOG_ERROR("get segfile info error!");
			return -1;
		}
		HLOG_DEBUG("segfile size: %lld", info->size);
		if (sizeof(struct inode_map_entry)  != storage->bs_file_pread(storage, file, 
							(char*)&im, sizeof(struct inode_map_entry) , 
							info->size - sizeof(struct inode_map_entry) )) {
			storage->bs_file_close(storage, file);
			HLOG_ERROR("pread error!");
			return -1;
		}
	} else {
		HLOG_ERROR("segfile not exist!");
		return -1;
	}
	storage->bs_file_close(storage, file);
	HLOG_DEBUG("im.inode_no:%lld,im.inode_addr:%lld(segno:%u,offset:%u)", 
			im.inode_no,im.inode_addr,get_segno(im.inode_addr),get_offset(im.inode_addr));
	HLOG_DEBUG("leave func %s", __func__);
	return im.inode_addr;
#endif
}

/*FIX IT ... */
static int restore_last_segno_file(const char* uri,const char*path){
	HLOG_DEBUG("enter func %s", __func__);
       int ret;
       const char* head,*hostname,*dir,*fs_name;
       int port;
	   HLOG_DEBUG("parse uri:%s",uri);
       ret = parse_from_uri(uri, (char **) &head, (char **) &hostname ,(char **) &dir, (char **) &fs_name, &port);
       if(ret !=0){
	       HLOG_DEBUG("parse uri error:%s",uri);
           return -1;
       }
	   struct sigaction sa,oldsa;
       sa.sa_handler = SIG_IGN;
       sigaction( SIGCHLD, &sa, &oldsa);
       //sigset_t mask,omask;
       //sigemptyset(&mask);
       //sigaddset(&mask,SIGCHLD);
       //if(sigprocmask(SIG_BLOCK,&mask,&omask)<0){
	   //    HLOG_DEBUG("SIGBLOCK ERROR");
       //    g_assert(0);
       //    return -1;
       //}
       const char full_segno_file_path[256];
       const char tmp_segno_file_path[256];
       const char *segfile = g_basename(path);
       sprintf((char *) full_segno_file_path,"%s/%s",uri,segfile);
       sprintf((char *) tmp_segno_file_path,"%s%s",full_segno_file_path,".bak");
       HLOG_DEBUG("full segno file path :%s",full_segno_file_path);
       const char cmd[256];
       pid_t status;
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s%s%s","hadoop fs"," -cp ",full_segno_file_path," ",tmp_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s","hadoop fs"," -rm ",full_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       memset((char *) cmd,0,256);
       sprintf((char *) cmd,"%s%s%s%s%s","hadoop fs"," -mv ",tmp_segno_file_path," ",full_segno_file_path);
       HLOG_DEBUG("cmd is  :%s",cmd);
       status = system(cmd);
       //sigprocmask(SIG_SETMASK,&omask,NULL);
       sigaction(SIGCHLD,&oldsa,0);
	   HLOG_DEBUG("leave func %s", __func__);
       return 0;
}

int get_cur_latest_segment_info(struct back_storage * storage,uint32_t *segno,uint32_t *offset){
    HLOG_DEBUG("enter func %s",__func__);
    int ret =0;
    uint32_t num_entries;
    bs_file_info_t * infos = storage->bs_file_list_dir(storage,".",&num_entries);
    if(infos == NULL){
       HLOG_ERROR("can not get fs:%s seg entries",storage->uri);
       return -1;
    }
    HLOG_DEBUG("how much file :%d",num_entries);
#if 0
	g_message("how much file :%d",num_entries);
#endif
    int i=0;
    const char* latest_file = NULL;
    uint64_t latest_st_mtime = 0;
    bs_file_info_t * info = infos;
    for(i=0;i<num_entries;i++){
        HLOG_DEBUG("file:%s,size:%llu,time:%llu\n",info->name,info->size,info->lmtime);  
        if(g_str_has_suffix(info->name,"seg")){
             if(latest_st_mtime < info->lmtime){
                latest_st_mtime = info->lmtime;
                latest_file = info->name;
                //*offset = info->size;
#if 0
                if(info->size == 0){
                    HLOG_DEBUG("meta data .................. not update ?");
                    ret = restore_last_segno_file(storage->uri,info->name); 
                    g_assert(0==ret);
                    bs_file_info_t * _info = storage->bs_file_info(storage,info->name);
                    *offset = _info->size;
                    free(_info);
                }
#endif
            }
        } 
        info++;
    }

    if(latest_file!=NULL){
        HLOG_DEBUG("latest file:%s",latest_file);
        const gchar *basename = g_basename(latest_file);
        gchar **v = g_strsplit(basename,".",2);
        *segno = atol(v[0]);
        HLOG_DEBUG("segno :%d",*segno);
        g_strfreev(v);
#if 1
        if(info->size == 0){
           HLOG_DEBUG("meta data .................. not update ?");
           ret = restore_last_segno_file(storage->uri,latest_file); 
           g_assert(0==ret);
           bs_file_info_t * _info = storage->bs_file_info(storage,latest_file);
           *offset = _info->size;
           free(_info);
        }
#endif
    }
out:
	HLOG_DEBUG("leave func %s", __func__);
    free(infos);
    return ret;   
}

/*
 * load_inode: load the inode into RAM.
 * @para ctrl: the global control of hlfs.
 * @para segfile: the segment file name.
 * @para inode_addr: the inode address.
 * @return: if error happens return NULL, if right return inode's RAM address.
 */
struct inode *load_inode(struct back_storage * storage,uint64_t inode_storage_addr)
{
	HLOG_DEBUG("enter func %s",__func__);
	bs_file_t file = NULL;
	struct inode *inode = (struct inode*)g_malloc0(sizeof(struct inode));
	if (!inode) {
		HLOG_ERROR("Allocate Error!");
		return NULL;
	}
	uint32_t offset = get_offset(inode_storage_addr); 
    const char segfile[SEGMENT_FILE_NAME_MAX];
	HLOG_DEBUG("seg num : %u", get_segno(inode_storage_addr));
    build_segfile_name(get_segno(inode_storage_addr),segfile);
	HLOG_DEBUG("segfile name : %s", segfile);
	HLOG_DEBUG("inode_addr %lld,offset %u", inode_storage_addr,offset);
	if (0 == storage->bs_file_is_exist(storage, segfile)) {
		if (NULL == (file = storage->bs_file_open(storage, segfile, BS_READONLY))) {
			g_free(inode);
			HLOG_ERROR("open segfile error!");
			return NULL;
		}
		if (sizeof(struct inode) != storage->bs_file_pread(storage, file,(char*)inode, sizeof(struct inode),offset)) {
			g_free(inode);
			storage->bs_file_close(storage, file);
			HLOG_ERROR("pread error!");
			return NULL;
		}
	} else {
		HLOG_ERROR("segfile not exist!");
		g_free(inode);
		return NULL;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return inode;
}


struct inode * load_latest_inode(struct back_storage *storage) 
{
    HLOG_DEBUG("enter func %s",__func__);
    uint32_t segno;
    uint32_t offset;
    int ret = get_cur_latest_segment_info(storage,&segno,&offset);
    if(ret !=0){
       return NULL;
    }
    HLOG_DEBUG("cur latest segno:%d",segno);
    uint64_t inode_storage_addr = get_last_inode_storage_addr_in_seg(storage,segno);
    HLOG_DEBUG("cur latest inode_storage_addr:%llu",inode_storage_addr);
    struct inode * latest_inode = load_inode(storage,inode_storage_addr); 
	HLOG_DEBUG("leave func %s", __func__);
    return latest_inode;
}

/*
 * get_db_addr: get the particular datablock number's address.
 * @para ctrl: the global control structure of hlfs.
 * @para inode: the inode structure.
 * @para db_no: the particular data block number.
 * @return: right return particular db's address, error return -1.
 */
uint64_t get_db_storage_addr_in_inode(struct back_storage * storage,
			struct inode *inode, uint64_t db_no,uint32_t block_size) 
{
    HLOG_DEBUG("enter func %s",__func__);
	uint64_t cur_storage_addr = 0;
    guint32 BLOCKSIZE = block_size;
	if (is_db_in_level1_index_range(db_no)) {
		int idx = db_no % 12;
		cur_storage_addr = inode->blocks[idx];
	} else if (is_db_in_level2_index_range(db_no)) {
		if (0 == inode->iblock) {
			return 1;
		}
		uint64_t *ib = (uint64_t *)read_block(storage, inode->iblock, BLOCKSIZE);
		if (NULL == ib) {
			return -1;
		}
		int idx = (db_no - 12) % 1024;
		cur_storage_addr = *(ib + idx);
		g_free(ib);
	} else if (is_db_in_level3_index_range(db_no)) {
		if(0 == inode->doubly_iblock) {
			return 1;
		}
		uint64_t *ib = (uint64_t *)read_block(storage, inode->doubly_iblock, BLOCKSIZE);
		if (NULL == ib) {
			return -1;
		}
		int idx = (db_no - 12 - 1024) / 1024;
		if (0 == *(ib + idx)) {
			return 1;
		}
		uint64_t *ib2 = (uint64_t *)read_block(storage, *(ib + idx), BLOCKSIZE);
		if (NULL == ib2) {
			return -1;
		}
		uint64_t idx2 = (db_no - 12 - 1024) % 1024;
		cur_storage_addr = *(ib2 + idx2);
		g_free(ib);
		g_free(ib2);
	} else if (is_db_in_level4_index_range(db_no)) {
		if(0 == inode->triply_iblock) {
			return 1;
		}
		uint64_t *ib = (uint64_t *)read_block(storage,inode->triply_iblock, BLOCKSIZE);
		if (NULL == ib) {
			return -1;
		}
		int idx = (db_no - 12 - 1024 - 1024 * 1024) / (1024 * 1024);
		if (0 == *(ib + idx)) {
			return 1;
		}
		uint64_t *ib2 = (uint64_t *)read_block(storage, *(ib + idx), BLOCKSIZE);
		if (NULL == ib2) {
			return -1;
		}
		uint64_t idx2 = (db_no - 12 - 1024 - 1024 * 1024) / 1024 % 1024;
		if (0 == *(ib2 + idx2)) {
			return 1;
		}
		uint64_t *ib3 = (uint64_t *)read_block(storage, *(ib2 + idx2), BLOCKSIZE);
		if (NULL == ib3) {
			return -1;
		}
		uint64_t idx3 = (db_no - 12 - 1024 - 1024 * 1024) % 1024;
		cur_storage_addr = *(ib3 + idx3);
		g_free(ib);
		g_free(ib2);
		g_free(ib3);
	} else {
		HLOG_ERROR("index error!");
		return -1;
	}
	HLOG_DEBUG("leave func %s", __func__);
	return cur_storage_addr;
}
uint64_t  SEGMENT_SIZE;
uint64_t  SEGMENT_SIZE_MASK = 0;
uint64_t  SEGMENT_SIZE_SHIFT = 0;
uint32_t  HBLOCK_SIZE;
int read_fs_meta(struct back_storage *storage,uint32_t *segment_size,uint32_t *block_size,uint32_t *max_fs_size)
{
	HLOG_DEBUG("enter func %s", __func__);
    int ret = 0;
    if(0!=storage->bs_file_is_exist(storage,"superblock")){
        HLOG_ERROR("superblock file can not be accessable");
        ret = -1;
        return ret;
    }
    char key_file_buf[4096];
    bs_file_t file = storage->bs_file_open(storage,"superblock",BS_READONLY);
    if(file == NULL){
      HLOG_ERROR("can not open superblock file");  
      ret = -1;
      return ret;
    }
    int size = storage->bs_file_pread(storage,file,key_file_buf,4096,0);
    if(size <0){
      HLOG_ERROR("can not read superblock file");
      storage->bs_file_close(storage,file);
      ret = -1;
      return ret;
    }
    GKeyFile * sb_keyfile = g_key_file_new();
    if(FALSE == g_key_file_load_from_data(sb_keyfile,key_file_buf,size,G_KEY_FILE_NONE,NULL)){
       HLOG_ERROR("superblock file format is not key value pairs");
       //storage->bs_file_close(storage,file);
       //g_key_file_free(sb_keyfile);
       ret = -2;
       goto out;
    }
    gchar * _uri =  g_key_file_get_string(sb_keyfile,"METADATA","uri",NULL);
    guint32 _seg_size = g_key_file_get_integer(sb_keyfile,"METADATA","segment_size",NULL);
    guint32 _block_size = g_key_file_get_integer(sb_keyfile,"METADATA","block_size",NULL);
    guint32 _max_fs_size = g_key_file_get_integer(sb_keyfile,"METADATA","max_fs_size",NULL);
    if(_uri==NULL || _seg_size == 0 || _block_size ==0 || _max_fs_size == 0){
       HLOG_ERROR("superblock parse error");
       g_free(_uri);
       ret = -3;
       goto out;
    }
    HLOG_DEBUG("superblock : seg size :%u block size :%u max fs size :%u(M) uri :%s\n",_seg_size,_block_size,_max_fs_size,_uri);
    *segment_size = _seg_size;
    *block_size = _block_size;
    *max_fs_size = _max_fs_size;
    if(0!=strcmp(storage->uri,_uri)){
        HLOG_ERROR("error fs name");
        g_free(_uri);
        ret = -4;
        goto out;
    }
    g_free(_uri);
    SEGMENT_SIZE = _seg_size;
    SEGMENT_SIZE_MASK  = SEGMENT_SIZE - 1;
    SEGMENT_SIZE_SHIFT = 0;
    while (0 != (SEGMENT_SIZE = (SEGMENT_SIZE >> 1)))
    {                                                                                                         
        SEGMENT_SIZE_SHIFT++;
    }
    HBLOCK_SIZE=_block_size;
out:
    g_key_file_free(sb_keyfile);
    storage->bs_file_close(storage,file);
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}
 

int load_latest_inode_map_entry(struct back_storage *storage,
			uint32_t segno, uint32_t last_offset,
				struct inode_map_entry *ime)
{
	HLOG_DEBUG("enter func %s", __func__);
   int ret = 0;
   const char segfile_name[SEGMENT_FILE_NAME_MAX];
   build_segfile_name(segno,segfile_name);
   bs_file_t file = storage->bs_file_open(storage,segfile_name,BS_READONLY); 
   if(file==NULL){
      HLOG_ERROR("can not open segment file %s",segfile_name);
      return -1; 
   }

   uint64_t inode_map_entry_pos = last_offset - 
                        sizeof(struct inode_map_entry);
   HLOG_DEBUG("inode map entry pos %llu",inode_map_entry_pos);
   if(sizeof(struct inode_map_entry) != 
                 storage->bs_file_pread(storage,
                 file,(char*)ime,
                 sizeof(struct inode_map_entry),
                 inode_map_entry_pos)){
       HLOG_ERROR("can not read inode map entry ");
       ret = -1;
       goto out1;
    }
out1:
    storage->bs_file_close(storage,file);
out2:
    //g_free(segfile_name);
    //g_free(segfile_path);
	HLOG_DEBUG("leave func %s", __func__);
    return ret;
}