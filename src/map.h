#ifndef FU_MAP_H
#define FU_MAP_H

#include "def.h"
#include "hash.h"

#define FuMap_INVALID_IDX FU_SIZE_MAX

typedef struct FuMap FuMap;
struct FuMap {
    fu_size_t len;
    fu_size_t cell_cap;
    fu_size_t item_cap;
    fu_size_t key_size;
    fu_size_t value_size;
    FuEqFn key_eq_fn;
    FuHashFn key_hash_fn;
    /* save idx to keys/values/hashes */
    fu_size_t *cells;
    /* save reverse idx of cells */
    fu_size_t *cell_idxs;
    fu_size_t *hashes;
    void *keys;
    void *values;
};

FuMap *FuMap_new(fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn);
FuMap *FuMap_with_capacity(fu_size_t capacity, fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn,
                           FuHashFn key_hash_fn);

void FuMap_init(FuMap *map, fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn);
void FuMap_deinit(FuMap *map);
void FuMap_deinit_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);
void FuMap_deinit_with_ptr(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);

void FuMap_drop(FuMap *map);
void FuMap_drop_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);
void FuMap_drop_with_ptrs(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);

void FuMap_clear(FuMap *map);
void FuMap_clear_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);
void FuMap_clear_with_ptrs(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn);

fu_size_t FuMap_len(FuMap *map);
fu_size_t FuMap_capacity(FuMap *map);
fu_bool_t FuMap_is_empty(FuMap *map);
FuMap *FuMap_copy(FuMap *map);

void *FuMap_get(FuMap *map, void *key);
void FuMap_set(FuMap *map, void *key, void *value, void *out_old_value);
void FuMap_remove(FuMap *map, void *key, void *out_old_value);
fu_bool_t FuMap_contains(FuMap *map, void *key);
FuMap *FuMap_clone(FuMap *map, FuCloneFn key_clone_fn, FuCloneFn value_clone_fn);
void FuMap_merge(FuMap *map, FuMap *other);

void *FuMap_get_ptr(FuMap *map, void *key_ptr);
void FuMap_set_ptr(FuMap *map, void *key_ptr, void *value_ptr, void **out_old_value_ptr);
void FuMap_remove_ptr(FuMap *map, void *key_ptr, void **out_old_value_ptr);
fu_bool_t FuMap_contains_ptr(FuMap *map, void *key_ptr);
FuMap *FuMap_clone_ptr(FuMap *map, FuCloneFn key_clone_fn, FuCloneFn value_clone_fn);
void FuMap_merge_ptr(FuMap *map, FuMap *other);

#endif /* FU_MAP_H */
