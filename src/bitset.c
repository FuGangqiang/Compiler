#include <string.h>

#include "alloc.h"
#include "bitset.h"

FuBitSet *FuBitSet_new() {
    FuBitSet *new = FuMem_alloc(sizeof(FuBitSet));
    FuBitSet_init(new);
    return new;
}

static void FuBitSet_make_room(FuBitSet *bs, fu_size_t n) {
    if (n != 0 && n <= bs->len * BITSET_CHUNK_SIZE) {
        return;
    }
    fu_size_t new_len;
    if (n == 0) {
        new_len = 1;
    } else {
        new_len = (n - 1) / BITSET_CHUNK_SIZE + 1;
    }
    fu_size_t *chunks = (fu_size_t *)FuMem_alloc(sizeof(fu_size_t) * new_len);
    memcpy(chunks, bs->chunks, bs->len * sizeof(fu_size_t));
    memset((char *)chunks + bs->len * sizeof(fu_size_t), 0, (new_len - bs->len) * sizeof(fu_size_t));
    FuMem_free(bs->chunks);
    bs->len = new_len;
    bs->chunks = chunks;
}

FuBitSet *FuBitSet_with_capacity(fu_size_t cap) {
    FuBitSet *new = FuMem_alloc(sizeof(FuBitSet));
    FuBitSet_init(new);
    FuBitSet_make_room(new, cap);
    return new;
}

void FuBitSet_init(FuBitSet *bs) {
    bs->len = 0;
    bs->chunks = NULL;
}

void FuBitSet_deinit(FuBitSet *bs) {
    if (bs->chunks != NULL) {
        FuMem_free(bs->chunks);
    }
}

void FuBitSet_drop(FuBitSet *bs) {
    if (!bs) {
        return;
    }
    FuBitSet_deinit(bs);
    FuMem_free(bs);
}

FuBitSet *FuBitSet_clone(FuBitSet *bs) {
    FuBitSet *new = FuBitSet_with_capacity(bs->len * BITSET_CHUNK_SIZE);
    memcpy((char *)new->chunks, (char *)bs->chunks, bs->len * sizeof(fu_size_t));
    return new;
}

fu_size_t FuBitSet_count(FuBitSet *bs) {
    fu_size_t i, n;

    n = 0;
    for (i = 0; i < bs->len * BITSET_CHUNK_SIZE; i++) {
        if (FuBitSet_contains(bs, i)) {
            n++;
        }
    }
    return n;
}

void FuBitSet_insert(FuBitSet *bs, fu_size_t n) {
    FuBitSet_make_room(bs, n);
    bs->chunks[n / BITSET_CHUNK_SIZE] |= 1ULL << (n % BITSET_CHUNK_SIZE);
}

void FuBitSet_remove(FuBitSet *bs, fu_size_t n) {
    if (n > bs->len * BITSET_CHUNK_SIZE) {
        return;
    }
    bs->chunks[n / BITSET_CHUNK_SIZE] &= ~(1ULL << (n % BITSET_CHUNK_SIZE));
}

fu_bool_t FuBitSet_contains(FuBitSet *bs, fu_size_t n) {
    if (n > bs->len * BITSET_CHUNK_SIZE) {
        return FU_FALSE;
    }
    return (bs->chunks[n / BITSET_CHUNK_SIZE] & (1ULL << (n % BITSET_CHUNK_SIZE))) != 0;
}

static void FuBitSet_make_len_eq(FuBitSet *bs1, FuBitSet *bs2) {
    if (bs1->len == bs2->len) {
        return;
    }
    if (bs1->len > bs2->len) {
        FuBitSet_make_room(bs2, bs1->len * BITSET_CHUNK_SIZE);
    }
    FuBitSet_make_room(bs1, bs2->len * BITSET_CHUNK_SIZE);
}

fu_bool_t FuBitSet_eq(FuBitSet *bs1, FuBitSet *bs2) {
    FuBitSet_make_len_eq(bs1, bs2);
#include <assert.h>
    assert(bs1->len == bs2->len);
    fu_size_t i;
    for (i = 0; i < bs1->len; i++) {
        if (bs1->chunks[i] != bs2->chunks[i]) {
            return FU_FALSE;
        }
    }
    return FU_TRUE;
}

void FuBitSet_union(FuBitSet *bs1, FuBitSet *bs2) {
    FuBitSet_make_len_eq(bs1, bs2);
    fu_size_t i;
    for (i = 0; i < bs1->len; i++) {
        bs1->chunks[i] |= bs2->chunks[i];
    }
}

void FuBitSet_intersect(FuBitSet *bs1, FuBitSet *bs2) {
    FuBitSet_make_len_eq(bs1, bs2);
    fu_size_t i;
    for (i = 0; i < bs1->len; i++) {
        bs1->chunks[i] &= bs2->chunks[i];
    }
}

void FuBitSet_diff(FuBitSet *bs1, FuBitSet *bs2) {
    FuBitSet_make_len_eq(bs1, bs2);
    fu_size_t i;
    for (i = 0; i < bs1->len; i++) {
        bs1->chunks[i] &= ~bs2->chunks[i];
    }
}

void FuBitSet_clear(FuBitSet *bs) {
    memset((char *)bs->chunks, 0, bs->len * sizeof(fu_size_t));
}
