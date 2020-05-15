#ifndef FU_SET_H
#define FU_SET_H

#include "def.h"
#include "hash.h"

#define FuSet_INVALID_IDX FU_SIZE_MAX

typedef struct FuSet FuSet;
struct FuSet {
    fu_size_t len;
    fu_size_t cell_cap;
    fu_size_t item_cap;
    fu_size_t key_size;
    FuEqFn key_eq_fn;
    FuHashFn key_hash_fn;
    /* save idx to keys/hashes */
    fu_size_t *cells;
    /* save reverse idx of cells */
    fu_size_t *cell_idxs;
    fu_size_t *hashes;
    void *keys;
};

FuSet *FuSet_new(fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn);
FuSet *FuSet_with_capacity(fu_size_t capacity, fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn);

void FuSet_init(FuSet *set, fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn);
void FuSet_deinit(FuSet *set);
void FuSet_deinit_with_values(FuSet *set, FuDropFn key_drop_fn);
void FuSet_deinit_with_ptr(FuSet *set, FuDropFn key_drop_fn);

void FuSet_drop(FuSet *set);
void FuSet_drop_with_values(FuSet *set, FuDropFn key_drop_fn);
void FuSet_drop_with_ptrs(FuSet *set, FuDropFn key_drop_fn);

void FuSet_clear(FuSet *set);
void FuSet_clear_with_values(FuSet *set, FuDropFn key_drop_fn);
void FuSet_clear_with_ptrs(FuSet *set, FuDropFn key_drop_fn);

fu_size_t FuSet_len(FuSet *set);
fu_size_t FuSet_capacity(FuSet *set);
fu_bool_t FuSet_is_empty(FuSet *set);
FuSet *FuSet_copy(FuSet *set);

void *FuSet_key_at(FuSet *set, fu_size_t idx);
void FuSet_add(FuSet *set, void *key, fu_size_t *out_idx);
void FuSet_remove(FuSet *set, void *key);
fu_bool_t FuSet_contains(FuSet *set, void *key, fu_size_t *out_idx);
FuSet *FuSet_clone(FuSet *set, FuCloneFn key_clone_fn);
void FuSet_merge(FuSet *set, FuSet *other);

void *FuSet_key_ptr_at(FuSet *set, fu_size_t idx);
void FuSet_add_ptr(FuSet *set, void *key_ptr, fu_size_t *out_idx);
void FuSet_remove_ptr(FuSet *set, void *key_ptr);
fu_bool_t FuSet_contains_ptr(FuSet *set, void *key_ptr, fu_size_t *out_idx);
FuSet *FuSet_clone_ptr(FuSet *set, FuCloneFn key_clone_fn);
void FuSet_merge_ptr(FuSet *set, FuSet *other);

#endif /* FU_SET_H */
