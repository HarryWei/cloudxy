/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _HLFS_CMD_DEFINE_H_
#define _HLFS_CMD_DEFINE_H_

#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <stdint.h>
#include <fcntl.h>
#include <stdlib.h>

typedef struct{
    enum ops_cmd {
	    start_clean =1,
            set_copy_waterlevel =2,
	    query_stat = 3
    } nbd_ops_cmd;
    char uri[128];
    int  value;
}NBD_OPS_CMD_T;

typedef struct{
    int err_no;
}NBD_OPS_RSP_T;

typedef struct {
    int is_start_clean;
    int copy_waterlevel;
}NBD_OPS_STAT;

#endif 
