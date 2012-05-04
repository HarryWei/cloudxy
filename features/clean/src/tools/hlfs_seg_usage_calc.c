#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include "storage_helper.h"
#include "seg_clean.h"
#include "comm_define.h"
#include "hlfs_log.h"
#include "api/hlfs.h"

static gchar *uri = NULL;
//static gchar *fsname = NULL;
static int mode=-1;
static gboolean verbose = FALSE;
static GOptionEntry entries[] = {
	    {"filesystem uri",'u', 0, G_OPTION_ARG_STRING,   &uri, "filesystem storage uri", "FSLOC"},
    	//{"filesystme name", 'f', 0, G_OPTION_ARG_FILENAME, &fsname, "filesystem name", "NAME"},
    	{"verbose", 'v', 0, G_OPTION_ARG_NONE, &verbose, "Be verbose", NULL },
    	{NULL}
};

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

/*segcalc.hlfs -u uri -v verbose*/
int main(int argc, char *argv[])
{
    GError *error = NULL;
    GOptionContext *context;
    context = g_option_context_new("- seg usage calc");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
            					(GOptionErrorFunc)error_func);
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
        g_message("option parsing failed: %s", error->message);
        exit(EXIT_FAILURE);
    }
    g_option_context_free(context);
    g_message("uri is :%s",uri);
    //g_message("fsname   is :%s",fsname);


    struct back_storage *storage = init_storage_handler(uri);
    if(NULL ==storage){
       g_message("can not get storage handler for uri:%s",uri);
       return -1;
    }
    g_message("storage init over....");
    uint32_t segment_size;
	uint32_t block_size;
    uint32_t max_fs_size;
	int ret = read_fs_meta(storage, &segment_size, &block_size,&max_fs_size);
    g_message("segment size:%d,block size:%d,max fs size%d",segment_size,block_size,max_fs_size);
#if 0
	SEGMENT_SIZE_MASK  = segment_size - 1;
    SEGMENT_SIZE_SHIFT = 0;
    while (0 != (segment_size = (segment_size >> 1)))
    {                                                                                                         
        SEGMENT_SIZE_SHIFT++;
    }
#endif
    struct inode * latest_inode = load_latest_inode(storage); 
    g_message("latest inode's timestamp :%llu",latest_inode->ctime);
    int num_entries;
    bs_file_info_t * infos = storage->bs_file_list_dir(storage,".",&num_entries);
    if(infos == NULL){
       g_message("can not get fs:%s seg entries",storage->uri);
       return -1;
    }
    g_message("there are %d files",num_entries);
    bs_file_info_t * info = infos;
    int i;
    GHashTable *seg_usage_hashtable = g_hash_table_new_full(g_direct_hash,g_direct_equal,NULL,NULL);//TODO
    ret = load_all_seg_usage(storage,SEGMENTS_USAGE_FILE,seg_usage_hashtable);
    g_assert(ret == 0 );

    uint32_t active_segno;
    uint32_t active_offset;
    ret = get_cur_latest_segment_info(storage,&active_segno,&active_offset);
    if(ret !=0){
        return -1;
    }
    g_message("active segno :%u",active_segno);
    for(i=0;i<num_entries;i++){
        g_message("file:%s,size:%llu,time:%llu",info->name,info->size,info->lmtime);  
        if(g_str_has_suffix(info->name,"seg")){
           g_message("valid seg file:%s",info->name);
           uint32_t segno = get_segfile_no(info->name);
           g_message("segno %u",segno);
           if(segno == active_segno){
              info++;
              continue; 
           }
           gpointer tmp = g_hash_table_lookup(seg_usage_hashtable,GINT_TO_POINTER(segno));
           struct segment_usage *seg_usage;
           if(tmp == NULL){
                g_message("can not find seg:%u in segment usage file",segno);
                seg_usage = (struct segment_usage*)g_malloc0(sizeof(struct segment_usage));
                memset(seg_usage,0,sizeof(struct segment_usage));
#if 0
    	        gchar * segusage_file = g_strconcat(info->name,".usage",NULL);
                dump_segment_usage(storage,segusage_file,&seg_usage);
#else
           }else{
                seg_usage = (struct segment_usage*)tmp;
           }
#endif 
           seg_usage_calc(storage,info->name,latest_inode,seg_usage,block_size);
           dump_seg_usage(storage,SEGMENTS_USAGE_FILE,seg_usage);

           g_free(seg_usage->bitmap);
           g_free(seg_usage);
        } 
        info++;
    }
    g_hash_table_destroy(seg_usage_hashtable);
    free(infos);
    return 0;
}
