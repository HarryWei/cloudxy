#ifndef _HLFS_CLEANER_ROUTE_H_
#define _HLFS_CLEANER_ROUTE_H_

#include <glib.h>
#include "hlfs_ctrl.h"
#include "comm_define.h"
#include "storage.h"
#include "segment_cleaner.h"

int rewrite_alive_blocks_from_seg(struct hlfs_ctrl *ctrl,struct segment_usage*seg_usage);


#endif 
