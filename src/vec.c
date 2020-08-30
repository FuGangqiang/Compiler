#include <assert.h>
#include <string.h>

#include "alloc.h"
#include "log.h"
#include "vec.h"

FuVec *FuVec_new(fu_size_t item_size) {
    FuVec *vec = FuMem_alloc(sizeof(FuVec));
    FuVec_init(vec, item_size);
    return vec;
}

FuVec *FuVec_with_capacity(fu_size_t capacity, fu_size_t item_size) {
    FuVec *vec = FuVec_new(item_size);
    FuVec_reserve(vec, capacity);
    return vec;
}

void FuVec_init(FuVec *vec, fu_size_t item_size) {
    vec->len = 0;
    vec->cap = 0;
    vec->item_size = item_size;
    vec->data = NULL;
}

void FuVec_deinit(FuVec *vec) {
    if (vec->data != NULL) {
        FuMem_free(vec->data);
    }
}

void FuVec_deinit_with_values(FuVec *vec, FuDropFn drop_fn) {
    fu_size_t len = FuVec_len(vec);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        void *item = FuVec_get(vec, i);
        drop_fn(item);
    }
}

void FuVec_deinit_with_ptrs(FuVec *vec, FuDropFn drop_fn) {
    fu_size_t len = FuVec_len(vec);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        void *ptr = FuVec_get_ptr(vec, i);
        drop_fn(ptr);
    }
}

void FuVec_drop(FuVec *vec) {
    if (!vec) {
        return;
    }
    FuVec_deinit(vec);
    FuMem_free(vec);
}

void FuVec_drop_with_values(FuVec *vec, FuDropFn drop_fn) {
    if (!vec) {
        return;
    }
    FuVec_deinit_with_values(vec, drop_fn);
    FuVec_drop(vec);
}

void FuVec_drop_with_ptrs(FuVec *vec, FuDropFn drop_fn) {
    if (!vec) {
        return;
    }
    FuVec_deinit_with_ptrs(vec, drop_fn);
    FuVec_drop(vec);
}

void FuVec_reserve(FuVec *vec, fu_size_t additional) {
    if (vec->cap - vec->len > additional) {
        return;
    }
    fu_size_t new_cap = vec->len + additional;
    void *new_data = FuMem_alloc(new_cap * vec->item_size);
    memcpy(new_data, vec->data, vec->len * vec->item_size);
    FuMem_free(vec->data);
    vec->cap = new_cap;
    vec->data = new_data;
}

void FuVec_make_room(FuVec *vec) {
    if (vec->cap > vec->len) {
        return;
    }
    /* vec->cap == vec->len */
    fu_size_t additional = vec->cap > 0 ? vec->cap : 4;
    FuVec_reserve(vec, additional);
}

fu_size_t FuVec_len(FuVec *vec) {
    return vec->len;
}

fu_size_t FuVec_capacity(FuVec *vec) {
    return vec->cap;
}

fu_bool_t FuVec_is_empty(FuVec *vec) {
    return FuVec_len(vec) == 0;
}

void FuVec_append(FuVec *vec, FuVec *other) {
    if (vec->item_size != other->item_size) {
        FATAL(NULL, "can not append diff vec type");
    }
    FuVec_reserve(vec, other->len);
    void *src = other->data;
    void *dest = vec->data + vec->len * vec->item_size;
    size_t bytes = other->len * other->item_size;
    memmove(dest, src, bytes);
    vec->len += other->len;
    other->len = 0;
}

FuVec *FuVec_copy(FuVec *vec) {
    FuVec *new = FuVec_with_capacity(vec->cap, vec->item_size);
    new->len = vec->len;
    memmove(new->data, vec->data, vec->len * vec->item_size);
    return new;
}

void *FuVec_get(FuVec *vec, fu_size_t i) {
    assert(vec->len > i);
    fu_size_t offset = i * vec->item_size;
    return (char *)vec->data + offset;
}

void FuVec_set(FuVec *vec, fu_size_t i, void *value, void *out_old) {
    assert(vec->len > i);
    fu_size_t offset = i * vec->item_size;
    void *item = (char *)vec->data + offset;
    if (out_old) {
        memmove(out_old, item, vec->item_size);
    }
    memmove(item, value, vec->item_size);
}

void *FuVec_first(FuVec *vec) {
    if (vec->len <= 0) {
        return NULL;
    }
    return FuVec_get(vec, 0);
}

void *FuVec_last(FuVec *vec) {
    if (vec->len <= 0) {
        return NULL;
    }
    return FuVec_get(vec, vec->len - 1);
}

void FuVec_push(FuVec *vec, void *value) {
    FuVec_make_room(vec);
    if (value) {
        fu_size_t offset = vec->len * vec->item_size;
        memcpy(vec->data + offset, value, vec->item_size);
        vec->len++;
    }
}

fu_err_t FuVec_pop(FuVec *vec, void *value) {
    if (vec->len <= 0) {
        return 1;
    }
    if (value) {
        void *item = FuVec_get(vec, vec->len - 1);
        memcpy(value, item, vec->item_size);
    }
    vec->len--;
    return 0;
}

void FuVec_remove(FuVec *vec, fu_size_t i, void *out_old) {
    if (i >= vec->len) {
        FATAL(NULL, "index outof range");
    }
    if (out_old) {
        void *item = FuVec_get(vec, i);
        memcpy(out_old, item, vec->item_size);
    }

    if (i == vec->len - 1) {
        vec->len--;
        return;
    }
    void *src = vec->data + (i + 1) * vec->item_size;
    void *dest = vec->data + i * vec->item_size;
    size_t bytes = (vec->len - i - 1) * vec->item_size;
    memmove(dest, src, bytes);
    vec->len--;
}

void FuVec_remove_slice(FuVec *vec, fu_size_t start, fu_size_t end, FuVec *out_old_slice) {
    if (start >= vec->len || end > vec->len) {
        FATAL(NULL, "index outof range");
    }
    if (start > end) {
        FATAL(NULL, "Vec slice start must <= end");
    }
    if (end == start) {
        return;
    }
    if (out_old_slice) {
        if (out_old_slice->item_size != vec->item_size) {
            FATAL(NULL, "Vec item_size not match");
        }
        fu_size_t i;
        for (i = start; i < end; i++) {
            void *item = FuVec_get(vec, i);
            FuVec_push(out_old_slice, item);
        }
    }
    if (end == vec->len) {
        vec->len = start;
        return;
    }
    void *src = vec->data + end * vec->item_size;
    void *dest = vec->data + start * vec->item_size;
    size_t bytes = (vec->len - end) * vec->item_size;
    memmove(dest, src, bytes);
    vec->len -= end - start;
}

void FuVec_insert(FuVec *vec, fu_size_t i, void *value) {
    if (i > vec->len) {
        FATAL(NULL, "index out of range");
    }
    FuVec_reserve(vec, 1);
    void *src = vec->data + i * vec->item_size;
    void *dest = vec->data + (i + 1) * vec->item_size;
    size_t bytes = (vec->len - i) * vec->item_size;
    memmove(dest, src, bytes);
    memmove(src, value, vec->item_size);
    vec->len++;
}

FuVec *FuVec_clone(FuVec *vec, FuCloneFn clone_fn) {
    FuVec *new = FuVec_with_capacity(vec->cap, vec->item_size);
    fu_size_t i;
    for (i = 0; i < vec->len; i++) {
        void *item = FuVec_get(vec, i);
        void *item_clone = clone_fn(item);
        FuVec_push(new, item_clone);
    }
    return new;
}

void *FuVec_get_ptr(FuVec *vec, fu_size_t i) {
    assert(vec->len > i);
    return *(void **)FuVec_get(vec, i);
}

void FuVec_set_ptr(FuVec *vec, fu_size_t i, void *ptr, void **out_old_ptr) {
    assert(vec->len > i);
    FuVec_set(vec, i, &ptr, out_old_ptr);
}

void *FuVec_first_ptr(FuVec *vec) {
    if (vec->len <= 0) {
        return NULL;
    }
    return FuVec_get_ptr(vec, 0);
}

void *FuVec_last_ptr(FuVec *vec) {
    if (vec->len <= 0) {
        return NULL;
    }
    return FuVec_get_ptr(vec, vec->len - 1);
}

fu_err_t FuVec_pop_ptr(FuVec *vec, void **out_ptr) {
    return FuVec_pop(vec, out_ptr);
}

void FuVec_push_ptr(FuVec *vec, void *ptr) {
    FuVec_push(vec, &ptr);
}

void FuVec_remove_ptr(FuVec *vec, fu_size_t i, void **out_old_ptr) {
    FuVec_remove(vec, i, out_old_ptr);
}

void FuVec_remove_ptr_slice(FuVec *vec, fu_size_t start, fu_size_t end, FuVec *out_old_ptr_slice) {
    FuVec_remove_slice(vec, start, end, out_old_ptr_slice);
}

void FuVec_insert_ptr(FuVec *vec, fu_size_t i, void *ptr) {
    FuVec_insert(vec, i, &ptr);
}

FuVec *FuVec_clone_ptr(FuVec *vec, FuCloneFn clone_fn) {
    FuVec *new = FuVec_with_capacity(vec->cap, vec->item_size);
    fu_size_t i;
    for (i = 0; i < vec->len; i++) {
        void *item = FuVec_get_ptr(vec, i);
        void *item_clone = clone_fn(item);
        FuVec_push_ptr(new, item_clone);
    }
    return new;
}

void FuVec_clear(FuVec *vec) {
    if (!vec) {
        return;
    }
    if (!vec->data) {
        return;
    }
    vec->len = 0;
}

void FuVec_clear_with_values(FuVec *vec, FuDropFn drop_fn) {
    fu_size_t len = FuVec_len(vec);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        void *item = FuVec_get(vec, i);
        drop_fn(item);
    }
    vec->len = 0;
}

void FuVec_clear_with_ptrs(FuVec *vec, FuDropFn drop_fn) {
    fu_size_t len = FuVec_len(vec);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        void *ptr = FuVec_get_ptr(vec, i);
        drop_fn(ptr);
    }
    vec->len = 0;
}
