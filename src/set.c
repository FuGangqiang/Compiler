#include <string.h>

#include "alloc.h"
#include "error.h"
#include "set.h"

FuSet *FuSet_new(fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn) {
    FuSet *set = FuMem_malloc(sizeof(FuSet));
    FuSet_init(set, key_size, key_eq_fn, key_hash_fn);
    return set;
}

FuSet *FuSet_with_capacity(fu_size_t capacity, fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn) {
    FuSet *set = FuMem_malloc(sizeof(FuSet));
    set->len = 0;
    set->cell_cap = capacity;
    set->item_cap = (fu_size_t)(capacity * 0.7f);
    set->key_size = key_size;
    set->key_eq_fn = key_eq_fn;
    set->key_hash_fn = key_hash_fn;
    set->cells = FuMem_malloc(set->cell_cap * sizeof(fu_size_t));
    set->cell_idxs = FuMem_malloc(set->item_cap * sizeof(fu_size_t));
    set->hashes = FuMem_malloc(set->item_cap * sizeof(fu_size_t));
    set->keys = FuMem_malloc(set->item_cap * set->key_size);

    fu_size_t i;
    for (i = 0; i < set->cell_cap; i++) {
        set->cells[i] = FuSet_INVALID_IDX;
    }
    return set;
}

void FuSet_init(FuSet *set, fu_size_t key_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn) {
    set->len = 0;
    set->cell_cap = 0;
    set->item_cap = 0;
    set->key_size = key_size;
    set->key_eq_fn = key_eq_fn;
    set->key_hash_fn = key_hash_fn;
    set->cells = NULL;
    set->cell_idxs = NULL;
    set->hashes = NULL;
    set->keys = NULL;
}

void FuSet_deinit(FuSet *set) {
    if (set->cells) {
        FuMem_free(set->cells);
    }
    if (set->cell_idxs) {
        FuMem_free(set->cell_idxs);
    }
    if (set->hashes) {
        FuMem_free(set->hashes);
    }
    if (set->keys) {
        FuMem_free(set->keys);
    }
}

void FuSet_deinit_with_values(FuSet *set, FuDropFn key_drop_fn) {
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        if (key_drop_fn) {
            void *key = FuSet_key_at(set, i);
            key_drop_fn(key);
        }
    }
}

void FuSet_deinit_with_ptr(FuSet *set, FuDropFn key_drop_fn) {
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        if (key_drop_fn) {
            void *key = FuSet_key_ptr_at(set, i);
            key_drop_fn(key);
        }
    }
}

void FuSet_drop(FuSet *set) {
    if (!set) {
        return;
    }
    FuSet_deinit(set);
    FuMem_free(set);
}

void FuSet_drop_with_values(FuSet *set, FuDropFn key_drop_fn) {
    if (!set) {
        return;
    }
    FuSet_deinit_with_values(set, key_drop_fn);
    FuSet_drop(set);
}

void FuSet_drop_with_ptrs(FuSet *set, FuDropFn key_drop_fn) {
    if (!set) {
        return;
    }
    FuSet_deinit_with_ptr(set, key_drop_fn);
    FuSet_drop(set);
}

void FuSet_clear(FuSet *set) {
    set->len = 0;
    fu_size_t i;
    for (i = 0; i < set->cell_cap; i++) {
        set->cells[i] = FuSet_INVALID_IDX;
    }
}

void FuSet_clear_with_values(FuSet *set, FuDropFn key_drop_fn) {
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        if (key_drop_fn) {
            void *key = FuSet_key_at(set, i);
            key_drop_fn(key);
        }
    }
    FuSet_clear(set);
}

void FuSet_clear_with_ptr(FuSet *set, FuDropFn key_drop_fn) {
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        if (key_drop_fn) {
            void *key = FuSet_key_ptr_at(set, i);
            key_drop_fn(key);
        }
    }
    FuSet_clear(set);
}

fu_size_t FuSet_len(FuSet *set) {
    return set->len;
}

fu_size_t FuSet_capacity(FuSet *set) {
    return set->cell_cap;
}

fu_bool_t FuSet_is_empty(FuSet *set) {
    return set->len == 0;
}

void *FuSet_key_at(FuSet *set, fu_size_t idx) {
    return (char *)set->keys + idx * set->key_size;
}

static void FuSet_set_key_at(FuSet *set, fu_size_t idx, void *key) {
    fu_size_t offset = idx * set->key_size;
    memcpy((char *)set->keys + offset, key, set->key_size);
}

static fu_size_t FuSet_hash_key(FuSet *set, void *key) {
    if (set->key_hash_fn) {
        return set->key_hash_fn(key);
    }
    return hash_bytes((fu_uint8_t *)key, set->key_size);
}

static fu_bool_t FuSet_eq_key(FuSet *set, void *key1, void *key2) {
    if (set->key_eq_fn) {
        return set->key_eq_fn(key1, key2);
    }
    return memcmp(key1, key2, set->key_size) == 0;
}

static void FuSet_make_room(FuSet *set) {
    if (set->len < set->item_cap) {
        return;
    }
    fu_size_t new_cap = set->cell_cap != 0 ? set->cell_cap * 2 : 8;
    FuSet *new = FuSet_with_capacity(new_cap, set->key_size, set->key_eq_fn, set->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        char *key = FuSet_key_at(set, i);
        FuSet_add(new, key, NULL);
    }

    FuMem_free(set->cells);
    FuMem_free(set->cell_idxs);
    FuMem_free(set->hashes);
    FuMem_free(set->keys);

    set->cell_cap = new->cell_cap;
    set->item_cap = new->item_cap;
    set->cells = new->cells;
    set->cell_idxs = new->cell_idxs;
    set->hashes = new->hashes;
    set->keys = new->keys;

    FuMem_free(new);
}

static fu_size_t FuSet_get_cell_idx(FuSet *set, void *key, fu_size_t hash, fu_bool_t *out_found) {
    *out_found = FU_FALSE;
    fu_size_t h = hash & (set->cell_cap - 1);
    fu_size_t i;
    for (i = 0; i < set->cell_cap; i++) {
        fu_size_t idx = (h + i) & (set->cell_cap - 1);
        fu_size_t item_idx = set->cells[idx];
        if (item_idx == FuSet_INVALID_IDX) {
            return idx;
        }
        if (hash != set->hashes[item_idx]) {
            continue;
        }
        void *check_key = FuSet_key_at(set, item_idx);
        if (FuSet_eq_key(set, key, check_key)) {
            *out_found = FU_TRUE;
            return idx;
        }
    }
    return FuSet_INVALID_IDX;
}

fu_bool_t FuSet_contains(FuSet *set, void *key, fu_size_t *out_idx) {
    fu_size_t hash = FuSet_hash_key(set, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuSet_get_cell_idx(set, key, hash, &found);
    if (out_idx) {
        *out_idx = set->cells[idx];
    }
    return found;
}

FuSet *FuSet_copy(FuSet *set) {
    FuSet *new = FuSet_with_capacity(set->cell_cap, set->key_size, set->key_eq_fn, set->key_hash_fn);
    new->len = set->len;
    if (set->len > 0) {
        memcpy(new->cells, set->cells, set->cell_cap * sizeof(fu_size_t));
        memcpy(new->cell_idxs, set->cell_idxs, set->item_cap * sizeof(fu_size_t));
        memcpy(new->hashes, set->hashes, set->item_cap * sizeof(fu_size_t));
        memcpy(new->keys, set->keys, set->item_cap * set->key_size);
    }
    return new;
}

void FuSet_add(FuSet *set, void *key, fu_size_t *out_idx) {
    FuSet_make_room(set);
    fu_size_t hash = FuSet_hash_key(set, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuSet_get_cell_idx(set, key, hash, &found);
    if (found) {
        fu_size_t item_idx = set->cells[idx];
        if (out_idx) {
            *out_idx = item_idx;
        }
        return;
    }
    set->cells[idx] = set->len;
    set->cell_idxs[set->len] = idx;
    set->hashes[set->len] = hash;
    FuSet_set_key_at(set, set->len, key);
    if (out_idx) {
        *out_idx = set->len;
    }
    set->len++;
}

void FuSet_remove(FuSet *set, void *key) {
    fu_size_t hash = FuSet_hash_key(set, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuSet_get_cell_idx(set, key, hash, &found);
    if (!found) {
        return;
    }

    fu_size_t item_idx = set->cells[idx];
    fu_size_t last_item_idx = set->len - 1;
    if (item_idx < last_item_idx) {
        void *last_key = FuSet_key_at(set, last_item_idx);
        FuSet_set_key_at(set, item_idx, last_key);
        set->cell_idxs[item_idx] = set->cell_idxs[last_item_idx];
        set->hashes[item_idx] = set->hashes[last_item_idx];
        set->cells[set->cell_idxs[item_idx]] = item_idx;
        /* set->cells[item_idx] = FuSet_INVALID_IDX, but need repire follower cells */
    }
    set->len--;

    fu_size_t i = idx;
    fu_size_t j = i;
    fu_size_t x;
    for (x = 0; x < set->cell_cap - 1; x++) {
        j = (j + 1) & (set->cell_cap - 1);
        if (set->cells[j] == FuSet_INVALID_IDX) {
            break;
        }
        fu_size_t k = set->hashes[set->cells[j]] & (set->cell_cap - 1);
        if ((j > i && (k <= i || k > j)) || (j < i && (k <= i && k > j))) {
            /* one case: j != 0, i == j - 1;  another case: j == 0, i == cell_cap - 1 */
            set->cell_idxs[set->cells[j]] = i;
            set->cells[i] = set->cells[j];
            i = j;
        }
    }
    set->cells[i] = FuSet_INVALID_IDX;
}

FuSet *FuSet_clone(FuSet *set, FuCloneFn key_clone_fn) {
    FuSet *new = FuSet_with_capacity(set->cell_cap, set->key_size, set->key_eq_fn, set->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        void *key = FuSet_key_at(set, i);
        void *new_key = key_clone_fn(key);
        FuSet_add(new, new_key, NULL);
    }
    return new;
}

void FuSet_merge(FuSet *to, FuSet *from) {
    fu_size_t i;
    for (i = 0; i < from->len; i++) {
        void *key = FuSet_key_at(from, i);
        FuSet_add(to, key, NULL);
    }
}

void *FuSet_key_ptr_at(FuSet *set, fu_size_t idx) {
    return *(void **)FuSet_key_at(set, idx);
}

void FuSet_add_ptr(FuSet *set, void *key_ptr, fu_size_t *out_idx) {
    FuSet_add(set, &key_ptr, out_idx);
}

void FuSet_remove_ptr(FuSet *set, void *key_ptr) {
    FuSet_remove(set, &key_ptr);
}

fu_bool_t FuSet_contains_ptr(FuSet *set, void *key, fu_size_t *out_idx) {
    return FuSet_contains(set, &key, out_idx);
}

FuSet *FuSet_clone_ptr(FuSet *set, FuCloneFn key_clone_fn) {
    FuSet *new = FuSet_with_capacity(set->cell_cap, set->key_size, set->key_eq_fn, set->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < set->len; i++) {
        void *key = FuSet_key_ptr_at(set, i);
        void *new_key = key_clone_fn(key);
        FuSet_add_ptr(new, new_key, NULL);
    }
    return new;
}

void FuSet_merge_ptr(FuSet *to, FuSet *from) {
    fu_size_t i;
    for (i = 0; i < from->len; i++) {
        void *key = FuSet_key_ptr_at(from, i);
        fu_size_t idx;
        FuSet_add_ptr(to, key, &idx);
    }
}
