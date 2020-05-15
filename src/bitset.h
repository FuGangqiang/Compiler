#ifndef FU_BITSET_H
#define FU_BITSET_H

#include <limits.h>

#include "def.h"

#define BITSET_CHUNK_SIZE (CHAR_BIT * sizeof(fu_size_t))

typedef struct FuBitSet FuBitSet;
struct FuBitSet {
    fu_size_t len;
    fu_size_t *chunks;
};

FuBitSet *FuBitSet_new();
FuBitSet *FuBitSet_with_capacity(fu_size_t cap);
void FuBitSet_init(FuBitSet *bs);
void FuBitSet_deinit(FuBitSet *bs);
void FuBitSet_drop(FuBitSet *bs);

FuBitSet *FuBitSet_clone(FuBitSet *bs);

fu_size_t FuBitSet_count(FuBitSet *bs);

void FuBitSet_insert(FuBitSet *bs, fu_size_t n);
void FuBitSet_remove(FuBitSet *bs, fu_size_t n);
fu_bool_t FuBitSet_contains(FuBitSet *bs, fu_size_t n);

fu_bool_t FuBitSet_eq(FuBitSet *bs1, FuBitSet *bs2);
void FuBitSet_union(FuBitSet *bs1, FuBitSet *bs2);
void FuBitSet_intersect(FuBitSet *bs1, FuBitSet *bs2);
void FuBitSet_diff(FuBitSet *bs1, FuBitSet *bs2);

void FuBitSet_clear(FuBitSet *bs);

#endif /* FU_BITSET_H */
