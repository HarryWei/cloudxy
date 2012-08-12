/*
  *  Copyright (C) 2012      KangHua <kanghua151@gmail.com> 
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

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
    uint64_t tmp = segment_no;
    tmp <<= SEGMENT_SIZE_SHIFT;
    *address = (*address & SEGMENT_SIZE_MASK) | tmp;
}

void set_offset(uint64_t *address, uint32_t offset)
{
    *address = (*address & ~SEGMENT_SIZE_MASK) | offset;
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
	
       //HLOG_DEBUG("ib_amount db_start is %d", db_start);
       //HLOG_DEBUG("ib_amount db_end is %d", db_end);
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
/* ibno=-1 mean this db not need this layer ib */
int get_layer1_ibno(uint32_t db_no){
       int ret = 0;
     uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
     if(is_db_in_level1_index_range(db_no)){
	    ret = -1;
     }else if (is_db_in_level2_index_range(db_no)){
           ret =  0;
     }else if (is_db_in_level3_index_range(db_no)){
           ret =  1;
     }else if (is_db_in_level4_index_range(db_no)){
           ret =  (1 + IB_ENTRY_NUM) + 1;
     }else{
           ret = -2;
     }
     return ret;
}

int get_layer2_ibno(uint32_t db_no){
     int ret = 0;
     uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
     if(is_db_in_level1_index_range(db_no)){
	    ret =  -1;
     }else if (is_db_in_level2_index_range(db_no)){
           ret =  -1;
     }else if (is_db_in_level3_index_range(db_no)){
           int idx = (db_no - 12 - IB_ENTRY_NUM)/IB_ENTRY_NUM;
           ret =  0 + 1 + idx;
     }else if (is_db_in_level4_index_range(db_no)){
           int idx = (db_no -12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM) / (IB_ENTRY_NUM*IB_ENTRY_NUM);
           ret =  0 +  ( 1 + IB_ENTRY_NUM ) + (1 + idx);
     }else{
           ret = -2;
     }
     return ret;
}

int get_layer3_ibno(uint32_t db_no){
     int ret = 0;
     uint32_t IB_ENTRY_NUM = HBLOCK_SIZE/sizeof(uint64_t);
     if(is_db_in_level1_index_range(db_no)){
	    ret =  -1;
     }else if (is_db_in_level2_index_range(db_no)){
           ret =  -1;
     }else if (is_db_in_level3_index_range(db_no)){
           ret =  -1;
     }else if (is_db_in_level4_index_range(db_no)){
           int idx = (db_no-12 - IB_ENTRY_NUM - IB_ENTRY_NUM*IB_ENTRY_NUM)/IB_ENTRY_NUM;
           ret =  0 + (1 +  IB_ENTRY_NUM)  + (1 + IB_ENTRY_NUM + idx);
     }else{
           ret = -2;
     }
     return ret;
}



