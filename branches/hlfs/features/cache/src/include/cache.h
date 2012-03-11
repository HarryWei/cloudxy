#include "api/hlfs.h"

typedef struct cache_entity {
	uint32_t block_size;
	uint32_t block_num;
	uint32_t dirty;
	char *block_in_mem;
} cache_ent_t;

typedef struct cache_ops {
	int (*find_block_num)(uint64_t pos, uint32_t len, uint32_t start_blk_num, \
			uint32_t end_blk_num);
} cache_ops_t;

int find_block_num(uint64_t pos, uint32_t len, uint32_t start_blk_num, \
		uint32_t end_blk_num);
