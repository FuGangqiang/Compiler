#include <string.h>

#include "alloc.h"
#include "log.h"
#include "map.h"

FuMap *FuMap_new(fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn) {
    FuMap *map = FuMem_malloc(sizeof(FuMap));
    FuMap_init(map, key_size, value_size, key_eq_fn, key_hash_fn);
    return map;
}

FuMap *FuMap_with_capacity(fu_size_t capacity, fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn,
                           FuHashFn key_hash_fn) {
    FuMap *map = FuMem_malloc(sizeof(FuMap));
    map->len = 0;
    map->cell_cap = capacity;
    map->item_cap = (fu_size_t)(capacity * 0.7f);
    map->key_size = key_size;
    map->value_size = value_size;
    map->key_eq_fn = key_eq_fn;
    map->key_hash_fn = key_hash_fn;
    map->cells = FuMem_malloc(map->cell_cap * sizeof(fu_size_t));
    map->cell_idxs = FuMem_malloc(map->item_cap * sizeof(fu_size_t));
    map->hashes = FuMem_malloc(map->item_cap * sizeof(fu_size_t));
    map->keys = FuMem_malloc(map->item_cap * map->key_size);
    map->values = FuMem_malloc(map->item_cap * map->value_size);

    fu_size_t i;
    for (i = 0; i < map->cell_cap; i++) {
        map->cells[i] = FuMap_INVALID_IDX;
    }
    return map;
}

void FuMap_init(FuMap *map, fu_size_t key_size, fu_size_t value_size, FuEqFn key_eq_fn, FuHashFn key_hash_fn) {
    map->len = 0;
    map->cell_cap = 0;
    map->item_cap = 0;
    map->key_size = key_size;
    map->value_size = value_size;
    map->key_eq_fn = key_eq_fn;
    map->key_hash_fn = key_hash_fn;
    map->cells = NULL;
    map->cell_idxs = NULL;
    map->hashes = NULL;
    map->keys = NULL;
    map->values = NULL;
}

void FuMap_deinit(FuMap *map) {
    if (map->cells) {
        FuMem_free(map->cells);
    }
    if (map->cell_idxs) {
        FuMem_free(map->cell_idxs);
    }
    if (map->hashes) {
        FuMem_free(map->hashes);
    }
    if (map->keys) {
        FuMem_free(map->keys);
    }
    if (map->values) {
        FuMem_free(map->values);
    }
}

static void *FuMap_key_at(FuMap *map, fu_size_t idx) {
    return (char *)map->keys + idx * map->key_size;
}

static void FuMap_set_key_at(FuMap *map, fu_size_t idx, void *key) {
    fu_size_t offset = idx * map->key_size;
    memcpy((char *)map->keys + offset, key, map->key_size);
}

static void *FuMap_value_at(FuMap *map, fu_size_t idx) {
    return (char *)map->values + idx * map->value_size;
}

static void FuMap_set_value_at(FuMap *map, fu_size_t idx, void *value) {
    fu_size_t offset = idx * map->value_size;
    memcpy((char *)map->values + offset, value, map->value_size);
}

static void *FuMap_key_ptr_at(FuMap *map, fu_size_t idx) {
    return *(void **)FuMap_key_at(map, idx);
}

static void *FuMap_value_ptr_at(FuMap *map, fu_size_t idx) {
    return *(void **)FuMap_value_at(map, idx);
}

void *FuMap_get_ptr(FuMap *map, void *key) {
    void *value = FuMap_get(map, &key);
    if (!value) {
        return NULL;
    }
    return *(void **)value;
}

void FuMap_set_ptr(FuMap *map, void *key_ptr, void *value_ptr, void **out_old_value_ptr) {
    FuMap_set(map, &key_ptr, &value_ptr, out_old_value_ptr);
}

void FuMap_deinit_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        if (key_drop_fn) {
            void *key = FuMap_key_at(map, i);
            key_drop_fn(key);
        }
        if (value_drop_fn) {
            void *value = FuMap_value_at(map, i);
            value_drop_fn(value);
        }
    }
}

void FuMap_deinit_with_ptr(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        if (key_drop_fn) {
            void *key = FuMap_key_ptr_at(map, i);
            key_drop_fn(key);
        }
        if (value_drop_fn) {
            void *value = FuMap_value_ptr_at(map, i);
            value_drop_fn(value);
        }
    }
}

void FuMap_drop(FuMap *map) {
    if (!map) {
        return;
    }
    FuMap_deinit(map);
    FuMem_free(map);
}

void FuMap_drop_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    if (!map) {
        return;
    }
    FuMap_deinit_with_values(map, key_drop_fn, value_drop_fn);
    FuMap_drop(map);
}

void FuMap_drop_with_ptrs(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    if (!map) {
        return;
    }
    FuMap_deinit_with_ptr(map, key_drop_fn, value_drop_fn);
    FuMap_drop(map);
}

void FuMap_clear(FuMap *map) {
    map->len = 0;
    fu_size_t i;
    for (i = 0; i < map->cell_cap; i++) {
        map->cells[i] = FuMap_INVALID_IDX;
    }
}

void FuMap_clear_with_values(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        if (key_drop_fn) {
            void *key = FuMap_key_at(map, i);
            key_drop_fn(key);
        }
        if (value_drop_fn) {
            void *value = FuMap_value_at(map, i);
            value_drop_fn(value);
        }
    }
    FuMap_clear(map);
}

void FuMap_clear_with_ptr(FuMap *map, FuDropFn key_drop_fn, FuDropFn value_drop_fn) {
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        if (key_drop_fn) {
            void *key = FuMap_key_ptr_at(map, i);
            key_drop_fn(key);
        }
        if (value_drop_fn) {
            void *value = FuMap_value_ptr_at(map, i);
            value_drop_fn(value);
        }
    }
    FuMap_clear(map);
}

fu_size_t FuMap_len(FuMap *map) {
    return map->len;
}

fu_size_t FuMap_capacity(FuMap *map) {
    return map->cell_cap;
}

fu_bool_t FuMap_is_empty(FuMap *map) {
    return map->len == 0;
}

FuMap *FuMap_copy(FuMap *map) {
    FuMap *new = FuMap_with_capacity(map->cell_cap, map->key_size, map->value_size, map->key_eq_fn, map->key_hash_fn);
    new->len = map->len;
    if (map->len > 0) {
        memcpy(new->cells, map->cells, map->cell_cap * sizeof(fu_size_t));
        memcpy(new->cell_idxs, map->cell_idxs, map->item_cap * sizeof(fu_size_t));
        memcpy(new->hashes, map->hashes, map->item_cap * sizeof(fu_size_t));
        memcpy(new->keys, map->keys, map->item_cap * map->key_size);
        memcpy(new->values, map->values, map->item_cap * map->value_size);
    }
    return new;
}

static fu_size_t FuMap_hash_key(FuMap *map, void *key) {
    if (map->key_hash_fn) {
        return map->key_hash_fn(key);
    }
    return hash_bytes((fu_uint8_t *)key, map->key_size);
}

static fu_bool_t FuMap_eq_key(FuMap *map, void *key1, void *key2) {
    if (map->key_eq_fn) {
        return map->key_eq_fn(key1, key2);
    }
    return memcmp(key1, key2, map->key_size) == 0;
}

static void FuMap_make_room(FuMap *map) {
    if (map->len < map->item_cap) {
        return;
    }
    fu_size_t new_cap = map->cell_cap != 0 ? map->cell_cap * 2 : 8;
    FuMap *new = FuMap_with_capacity(new_cap, map->key_size, map->value_size, map->key_eq_fn, map->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        char *key = FuMap_key_at(map, i);
        char *value = FuMap_value_at(map, i);
        FuMap_set(new, key, value, NULL);
    }

    FuMem_free(map->cells);
    FuMem_free(map->cell_idxs);
    FuMem_free(map->hashes);
    FuMem_free(map->keys);
    FuMem_free(map->values);

    map->cell_cap = new->cell_cap;
    map->item_cap = new->item_cap;
    map->cells = new->cells;
    map->cell_idxs = new->cell_idxs;
    map->hashes = new->hashes;
    map->keys = new->keys;
    map->values = new->values;

    FuMem_free(new);
}

static fu_size_t FuMap_get_cell_idx(FuMap *map, void *key, fu_size_t hash, fu_bool_t *out_found) {
    *out_found = FU_FALSE;
    fu_size_t h = hash & (map->cell_cap - 1);
    fu_size_t i;
    for (i = 0; i < map->cell_cap; i++) {
        fu_size_t idx = (h + i) & (map->cell_cap - 1);
        fu_size_t item_idx = map->cells[idx];
        if (item_idx == FuMap_INVALID_IDX) {
            return idx;
        }
        if (hash != map->hashes[item_idx]) {
            continue;
        }
        void *check_key = FuMap_key_at(map, item_idx);
        if (FuMap_eq_key(map, key, check_key)) {
            *out_found = FU_TRUE;
            return idx;
        }
    }
    return FuMap_INVALID_IDX;
}

void *FuMap_get(FuMap *map, void *key) {
    fu_size_t hash = FuMap_hash_key(map, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuMap_get_cell_idx(map, key, hash, &found);
    if (!found) {
        return NULL;
    }
    fu_size_t item_idx = map->cells[idx];
    return FuMap_value_at(map, item_idx);
}

void FuMap_set(FuMap *map, void *key, void *value, void *out_old_value) {
    FuMap_make_room(map);
    fu_size_t hash = FuMap_hash_key(map, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuMap_get_cell_idx(map, key, hash, &found);
    if (found) {
        fu_size_t item_idx = map->cells[idx];
        if (out_old_value) {
            char *v = FuMap_value_at(map, item_idx);
            memcpy(out_old_value, v, map->value_size);
        }
        FuMap_set_value_at(map, item_idx, value);
        return;
    }
    map->cells[idx] = map->len;
    map->cell_idxs[map->len] = idx;
    map->hashes[map->len] = hash;
    FuMap_set_key_at(map, map->len, key);
    FuMap_set_value_at(map, map->len, value);
    map->len++;
    return;
}

void FuMap_remove(FuMap *map, void *key, void *out_old_value) {
    fu_size_t hash = FuMap_hash_key(map, key);
    fu_bool_t found = FU_FALSE;
    fu_size_t idx = FuMap_get_cell_idx(map, key, hash, &found);
    if (!found) {
        return;
    }

    fu_size_t item_idx = map->cells[idx];
    if (out_old_value) {
        void *value = FuMap_value_at(map, item_idx);
        memcpy(out_old_value, value, map->value_size);
    }

    fu_size_t last_item_idx = map->len - 1;
    if (item_idx < last_item_idx) {
        void *last_key = FuMap_key_at(map, last_item_idx);
        FuMap_set_key_at(map, item_idx, last_key);
        void *last_value = FuMap_value_at(map, last_item_idx);
        FuMap_set_value_at(map, item_idx, last_value);
        map->cell_idxs[item_idx] = map->cell_idxs[last_item_idx];
        map->hashes[item_idx] = map->hashes[last_item_idx];
        map->cells[map->cell_idxs[item_idx]] = item_idx;
        /* map->cells[item_idx] = FuMap_INVALID_IDX, but need repire follower cells */
    }
    map->len--;

    fu_size_t i = idx;
    fu_size_t j = i;
    fu_size_t x;
    for (x = 0; x < map->cell_cap - 1; x++) {
        j = (j + 1) & (map->cell_cap - 1);
        if (map->cells[j] == FuMap_INVALID_IDX) {
            break;
        }
        fu_size_t k = map->hashes[map->cells[j]] & (map->cell_cap - 1);
        if ((j > i && (k <= i || k > j)) || (j < i && (k <= i && k > j))) {
            /* one case: j != 0, i == j - 1;  another case: j == 0, i == cell_cap - 1 */
            map->cell_idxs[map->cells[j]] = i;
            map->cells[i] = map->cells[j];
            i = j;
        }
    }
    map->cells[i] = FuMap_INVALID_IDX;
}

fu_bool_t FuMap_contains(FuMap *map, void *key) {
    fu_size_t hash = FuMap_hash_key(map, key);
    fu_bool_t found = FU_FALSE;
    FuMap_get_cell_idx(map, key, hash, &found);
    return found;
}

FuMap *FuMap_clone(FuMap *map, FuCloneFn key_clone_fn, FuCloneFn value_clone_fn) {
    FuMap *new = FuMap_with_capacity(map->cell_cap, map->key_size, map->value_size, map->key_eq_fn, map->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        void *key = FuMap_key_at(map, i);
        void *value = FuMap_value_at(map, i);
        void *new_key = key_clone_fn(key);
        void *new_value = value_clone_fn(value);
        FuMap_set(new, new_key, new_value, NULL);
    }
    return new;
}

/* value map: can not merge ptr, do not return the old_values */
void FuMap_merge(FuMap *to, FuMap *from) {
    fu_size_t i;
    for (i = 0; i < from->len; i++) {
        void *key = FuMap_key_at(from, i);
        void *value = FuMap_value_at(from, i);
        FuMap_set(to, key, value, NULL);
    }
}

void FuMap_remove_ptr(FuMap *map, void *key_ptr, void **out_old_value_ptr) {
    FuMap_remove(map, &key_ptr, out_old_value_ptr);
}

fu_bool_t FuMap_contains_ptr(FuMap *map, void *key_ptr) {
    return FuMap_contains(map, &key_ptr);
}

void FuMap_merge_ptr(FuMap *to, FuMap *from) {
    fu_size_t i;
    for (i = 0; i < from->len; i++) {
        void *key = FuMap_key_ptr_at(from, i);
        void *value = FuMap_value_ptr_at(from, i);
        FuMap_set_ptr(to, key, value, NULL);
    }
}

FuMap *FuMap_clone_ptr(FuMap *map, FuCloneFn key_clone_fn, FuCloneFn value_clone_fn) {
    FuMap *new = FuMap_with_capacity(map->cell_cap, map->key_size, map->value_size, map->key_eq_fn, map->key_hash_fn);
    fu_size_t i;
    for (i = 0; i < map->len; i++) {
        void *key = FuMap_key_ptr_at(map, i);
        void *value = FuMap_value_ptr_at(map, i);
        void *new_key = key_clone_fn(key);
        void *new_value = value_clone_fn(value);
        FuMap_set_ptr(new, new_key, new_value, NULL);
    }
    return new;
}
