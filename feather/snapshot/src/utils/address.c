#include <time.h>
#include <stdint.h>
#include <glib.h>
#include "comm_define.h"
#include "hlfs_log.h"

uint32_t get_segno(uint64_t address)
{
    return (uint32_t) (address >> SEGMENT_SIZE_SHIFT);
}

uint32_t get_offset(uint64_t address)
{
    return (uint32_t) (address & SEGMENT_SIZE_MASK);
}

void set_segno(uint64_t *address, uint32_t segment_no)
{
	HLOG_DEBUG("harry dbg *address is %llu", *address);
	HLOG_DEBUG("harry dbg segno is %u", segment_no);
    uint64_t tmp = segment_no;
    tmp <<= SEGMENT_SIZE_SHIFT;
    *address = (*address & SEGMENT_SIZE_MASK) | tmp;
	HLOG_DEBUG("harry dbg after set segno *address is %llu", *address);
}

void set_offset(uint64_t *address, uint32_t offset)
{
	HLOG_DEBUG("harry dbg *address is %llu", *address);
    *address = (*address & ~SEGMENT_SIZE_MASK) | offset;
	HLOG_DEBUG("harry dbg after set, *address is %llu", *address);
} 

gboolean is_db_in_level1_index_range(uint32_t db_no)
{
        if(db_no<12 && db_no >=0) {
            return TRUE;
	} else {
            return FALSE;
	}
}

gboolean is_db_in_level2_index_range(uint32_t db_no)
{
        uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
        if(db_no < (IB_ENTRY_NUM + 12) && db_no >= 12) {
            return TRUE;
	} else {
            return FALSE;
	}
}

gboolean is_db_in_level3_index_range(uint32_t db_no)
{
        uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
         if(db_no<(IB_ENTRY_NUM*IB_ENTRY_NUM + IB_ENTRY_NUM + 12 ) && db_no >= (IB_ENTRY_NUM+12)) {
            return TRUE;
	} else {
            return FALSE;
	}
}

gboolean is_db_in_level4_index_range(uint32_t db_no)
{
         uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
         if(db_no < (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM + IB_ENTRY_NUM*IB_ENTRY_NUM + IB_ENTRY_NUM + 12) && db_no >= ((IB_ENTRY_NUM*IB_ENTRY_NUM) + IB_ENTRY_NUM + 12)) {
            return TRUE;
	} else {
            return FALSE;
	}
}

uint32_t ib_amount(uint32_t db_start, uint32_t db_end)
{
#if 1
       uint32_t ib_amount = 0;
       uint32_t db_cur_no = 0;
       uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
	
       HLOG_DEBUG("ib_amount db_start is %d", db_start);
       HLOG_DEBUG("ib_amount db_end is %d", db_end);
       for (db_cur_no = db_start; db_cur_no <= db_end; db_cur_no++) {
               if (is_db_in_level1_index_range(db_cur_no)) {
                       ;
               } else if (is_db_in_level2_index_range(db_cur_no)) {
                       if( (db_cur_no - 12 + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end ){
                               ib_amount += 1;
                       }
               } else if (is_db_in_level3_index_range(db_cur_no)) {
                       if((db_cur_no -12 - IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                               ib_amount += 1;
                       }
		       if ((db_cur_no - 12 -IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
                               ib_amount += 1;
                       }
               } else if (is_db_in_level4_index_range(db_cur_no)) {
                       if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % IB_ENTRY_NUM == 0 || db_cur_no == db_end){
                               ib_amount += 1;
                       } 
		       if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM * IB_ENTRY_NUM)  == 0 || db_cur_no == db_end){
                               ib_amount += 1;
                       } 
		       if((db_cur_no-12-IB_ENTRY_NUM-IB_ENTRY_NUM*IB_ENTRY_NUM + 1) % (IB_ENTRY_NUM*IB_ENTRY_NUM*IB_ENTRY_NUM) == 0 || db_cur_no == db_end){
                               ib_amount += 1;
                       }
               }
       }
       HLOG_DEBUG("ib_amount we need %d ibs", ib_amount);
       return ib_amount;
#endif
}
