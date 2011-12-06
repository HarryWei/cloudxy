#ifndef _HLFS_LOG_H
#define _HLFS_LOG_H

#include "log4c.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define LOG_LEN				(1024)
#define LOG_CONFIG_FILE_PATH		"../../output/bin/log4crc"

#define HLOG_NOTICE(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_NOTICE, __msg_log, ##args);	\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_TRACE(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_TRACE, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_FATAL(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_FATAL, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_DEBUG(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_DEBUG, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_INFO(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_INFO, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_ERROR(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_ERROR, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}

#define HLOG_WARN(msg, args...) { 										\
	if (g_file_test(LOG_CONFIG_FILE_PATH, G_FILE_TEST_EXISTS)) {						\
		if (NULL == __mycat) {										\
			__mycat = log4c_category_get("hlfslog");						\
		}												\
		memset(__msg_log, 0, LOG_LEN);									\
		snprintf(__msg_log, LOG_LEN, "[%s][%s][%d]%s", __FILE__, __func__, __LINE__, msg);		\
		log4c_category_log_locinfo(__mycat, NULL, LOG4C_PRIORITY_WARN, __msg_log, ##args);		\
	} else {												\
		printf(msg, ##args);										\
		printf("\n");											\
	}													\
}
static char __msg_log[LOG_LEN];
static log4c_category_t *__mycat;
#endif
