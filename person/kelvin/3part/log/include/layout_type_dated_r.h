/* $Id$
 *
 * layout_type_dated_r.h
 *
 * Copyright 2001-2003, Meiosys (www.meiosys.com). All rights reserved.
 *
 * See the COPYING file for the terms of usage and distribution.
 */

#ifndef log4c_layout_type_dated_r_h
#define log4c_layout_type_dated_r_h

/**
 * @file layout_type_dated_r.h
 *
 * @brief Implement a dated_r layout.
 *
 * In @c log4j.PatternLayout conventions, the dated_r layout has the following
 * conversion pattern: @c "%d %P %c - %m\n".
 *
 * Where
 * @li @c "%d" is the date of the logging event
 * @li @c "%P" is the priority of the logging event
 * @li @c "%c" is the category of the logging event
 * @li @c "%m" is the application supplied message associated with the
 * logging event
 *
 *
 *
 **/

#include <defs.h>
#include <layout.h>

__LOG4C_BEGIN_DECLS

extern const log4c_layout_type_t log4c_layout_type_dated_r;

__LOG4C_END_DECLS

#endif
