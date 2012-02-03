#ifndef _HLFS_ADDRESS_H_
#define _HLFS_ADDRESS_H_

#include <time.h>
#include <stdint.h>
#include <glib.h>

uint32_t get_segno(uint64_t address);
uint32_t get_offset(uint64_t address);
void set_offset(uint64_t *address, uint32_t offset);
void set_segno(uint64_t *address, uint32_t segment_no);
gboolean is_db_in_level1_index_range(uint32_t db_no);
gboolean is_db_in_level2_index_range(uint32_t db_no);
gboolean is_db_in_level3_index_range(uint32_t db_no);
gboolean is_db_in_level4_index_range(uint32_t db_no);
uint32_t ib_amount(uint32_t db_start, uint32_t db_end);

#endif
