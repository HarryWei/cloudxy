#ifndef _COMM_DEFINE_H_
#define _COMM_DEFINE_H_
#include <stdint.h>
#define G_LOG_HLFS_DOMAIN   "HLFS"
#define HLFS_INODE_NO  (77)
#define SEGMENT_FILE_NAME_MAX (64)

//#define SEGMENT_SIZE        (64*1024*1024UL)
//#define SEGMENT_SIZE_MASK   (SEGMENT_SIZE-1)  
//#define SEGMENT_SIZE_SHIFT  26 
extern uint64_t SEGMENT_SIZE;
extern uint64_t SEGMENT_SIZE_MASK;
extern uint64_t SEGMENT_SIZE_SHIFT;
extern uint32_t HBLOCK_SIZE;

#endif
