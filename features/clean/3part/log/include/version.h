/* $Id$
 *
 * version.h
 * 
 * Copyright 2001-2003, Meiosys (www.meiosys.com). All rights reserved.
 *
 * See the COPYING file for the terms of usage and distribution.
 */

#ifndef log4c_version_h
#define log4c_version_h

/**
 * @file version.h
 *
 * @brief log4c version information
 **/

#include <defs.h>

__LOG4C_BEGIN_DECLS

/**
 * constant macro holding the major version of log4c
 **/
#define LOG4C_MAJOR_VERSION 1
/**
 * constant macro holding the minor version of log4c
 **/
#define LOG4C_MINOR_VERSION 2
/**
 * constant macro holding the micro version of log4c
 **/
#define LOG4C_MICRO_VERSION 1

/**
 * constant variable holding the major version of log4c
 **/
extern const int log4c_major_version;
/**
 * constant variable holding the minor version of log4c
 **/
extern const int log4c_minor_version;
/**
 * constant variable holding the micro version of log4c
 **/
extern const int log4c_micro_version;

/**
 * @return a string containing the full log4c version
 **/
extern const char* log4c_version(void);

__LOG4C_END_DECLS

#endif

