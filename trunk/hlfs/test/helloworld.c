#include <stdio.h>
#include <stdlib.h>

#include "log4c.h"

#define LOG_DEBUG(category, msg, args...) {	\
	const log4c_location_info_t locinfo = LOG4C_LOCATION_INFO_INITIALIZER(NULL);	\
	log4c_category_log_locinfo(mycat, &locinfo, LOG4C_PRIORITY_DEBUG, msg, ##args);	\
}


int main(int argc, char** argv){
  int rc = 0;
  log4c_category_t* mycat = NULL;
  
  if (log4c_init()){
    printf("log4c_init() failed");
    rc = 1;  
  }else{
      mycat = log4c_category_get("loghlfs");

      log4c_category_log(mycat,LOG4C_PRIORITY_DEBUG,"[file - %s] [func - %s] [line - %d]: Hello Wrold", __FILE__, __func__, __LINE__);
//      LOG_DEBUG(mycat, "Our land! func %s, i %d", __func__, rc);
    
    /* Explicitly call the log4c cleanup routine */
    if ( log4c_fini()){
      printf("log4c_fini() failed");
    }
  }
  return 0;
}
