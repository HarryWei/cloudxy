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

int hlfs_fopen(struct hlfs_ctrl *ctrl, const char *f_path, int flag) {
	g_message("9999 enter func %s", __func__);
    if(ctrl == NULL || f_path ==NULL){
		HLOG_ERROR("parameter error!");
        return -1;
    }
    int ret = 0;
	
	return ret;
}
