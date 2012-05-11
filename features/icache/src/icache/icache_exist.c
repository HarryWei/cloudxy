#include "hlfs_log.h"
#include "glib.h"
#include "icache.h"
gboolean  icache_iblock_exist(ICACHE_CTRL *icache_ctrl, uint64_t block_no){
    HLOG_DEBUG("--Entering func %s", __func__);
    int ret = 0;
    iblock_t * iblock = icache_query(icache_ctrl,block_no);
    if(iblock==NULL){
       return FALSE;
    }
    return TRUE;
}
