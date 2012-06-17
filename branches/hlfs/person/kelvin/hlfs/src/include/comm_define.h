/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#ifndef _COMM_DEFINE_H_
#define _COMM_DEFINE_H_
#include <stdint.h>
#define G_LOG_HLFS_DOMAIN   "HLFS"
#define HLFS_INODE_NO  77U
#define SEGMENT_FILE_NAME_MAX  64U
#define HLFS_FILE_NAME_MAX  64U

//#define SEGMENT_SIZE        (64*1024*1024UL)
//#define SEGMENT_SIZE_MASK   (SEGMENT_SIZE-1)  
//#define SEGMENT_SIZE_SHIFT  26 
//#define HBLOCK_SIZE			(8*1024UL)
extern uint64_t SEGMENT_SIZE;
extern uint64_t SEGMENT_SIZE_MASK;
extern uint64_t SEGMENT_SIZE_SHIFT;
extern uint32_t HBLOCK_SIZE;

/* hlfs first start */
#define		HLFS_FS			((int) (1))

/*
 * Following are some error numbers in hlfs, you can also add 
 * the error number, which you think it is worthy. But when
 * you add yours, please add the comments for this error number.
 * You'd better send a mail to cloudxy@googlegroups.com for your
 * error number before you do this, which can make sure your error 
 * number is ok for all the developers, thanks.
 */
#define EHLFS_MEM			((int) (1)) 		/* memory error in hlfs */
#define EHLFS_NOFILE 		((int) (2)) 		/* no this file */
#define EHLFS_FUNC			((int) (3)) 		/* invoke func error */
#define EHLFS_SSEXIST		((int) (4)) 		/* snapshot name exist */
#define EHLFS_SSNOTEXIST 	((int) (5))		/* snapshot name not exist */
#define EHLFS_PERM 			((int) (6)) 		/* Permssion deny*/
#define EHLFS_PARAM			((int) (7)) 		/* Parameter error*/
#define EHLFS_NOITEM		((int) (8)) 		/* NO item in one data structure*/
#define EHLFS_UNKNOWN		((int) (99)) 		/* unknown error */

#endif
