#ifndef FU_VEC_H
#define FU_VEC_H

#include "def.h"

typedef struct FuVec FuVec;
struct FuVec {
    fu_size_t len;
    fu_size_t cap;
    fu_size_t item_size;
    void *data;
};

FuVec *FuVec_new(fu_size_t item_size);
FuVec *FuVec_with_capacity(fu_size_t capacity, fu_size_t item_size);
void FuVec_init(FuVec *vec, fu_size_t item_size);
void FuVec_deinit(FuVec *vec);
void FuVec_deinit_with_values(FuVec *vec, FuDropFn drop_fn);
void FuVec_deinit_with_ptrs(FuVec *vec, FuDropFn drop_fn);
void FuVec_drop(FuVec *vec);
void FuVec_drop_with_values(FuVec *vec, FuDropFn drop_fn);
void FuVec_drop_with_ptrs(FuVec *vec, FuDropFn drop_fn);

void FuVec_reserve(FuVec *vec, fu_size_t additional);
void FuVec_make_room(FuVec *vec);

fu_size_t FuVec_len(FuVec *vec);
fu_size_t FuVec_capacity(FuVec *vec);
fu_bool_t FuVec_is_empty(FuVec *vec);
void FuVec_append(FuVec *vec, FuVec *other);
FuVec *FuVec_copy(FuVec *vec);

void *FuVec_get(FuVec *vec, fu_size_t i);
void FuVec_set(FuVec *vec, fu_size_t i, void *value, void *out_old);
void *FuVec_first(FuVec *vec);
void *FuVec_last(FuVec *vec);
void FuVec_push(FuVec *vec, void *value);
fu_err_t FuVec_pop(FuVec *vec, void *value);
void FuVec_remove(FuVec *vec, fu_size_t i, void *out_old);
void FuVec_remove_slice(FuVec *vec, fu_size_t start, fu_size_t end, FuVec *out_old_slice);
void FuVec_insert(FuVec *vec, fu_size_t i, void *value);
FuVec *FuVec_clone(FuVec *vec, FuCloneFn clone_fn);

void *FuVec_get_ptr(FuVec *vec, fu_size_t i);
void FuVec_set_ptr(FuVec *vec, fu_size_t i, void *value, void **out_old_ptr);
void *FuVec_first_ptr(FuVec *vec);
void *FuVec_last_ptr(FuVec *vec);
void FuVec_push_ptr(FuVec *vec, void *ptr);
fu_err_t FuVec_pop_ptr(FuVec *vec, void **out_ptr);
void FuVec_remove_ptr(FuVec *vec, fu_size_t i, void **out_old_ptr);
void FuVec_remove_ptr_slice(FuVec *vec, fu_size_t start, fu_size_t end, FuVec *out_old_ptr_slice);
void FuVec_insert_ptr(FuVec *vec, fu_size_t i, void *ptr);
FuVec *FuVec_clone_ptr(FuVec *vec, FuCloneFn clone_fn);

void FuVec_clear(FuVec *vec);
void FuVec_clear_with_values(FuVec *vec, FuDropFn drop_fn);
void FuVec_clear_with_ptrs(FuVec *vec, FuDropFn drop_fn);

#endif /* FU_VEC_H */
