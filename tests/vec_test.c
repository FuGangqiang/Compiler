#include <assert.h>

#include "vec.h"

void test_vec(void) {
    int err;
    fu_size_t i;

    char arr[] = {'a', 'b', 'c', 'd', 'e'};
    fu_size_t arr_len = sizeof(arr) / sizeof(arr[0]);

    FuVec *vec = FuVec_new(sizeof(arr[0]));
    assert(FuVec_is_empty(vec) == FU_TRUE);

    for (i = 0; i < arr_len; i++) {
        FuVec_push(vec, arr + i);
        assert(vec->len == i + 1);
    }

    assert(FuVec_capacity(vec) == 8);
    for (i = 0; i < arr_len; i++) {
        char *value = FuVec_get(vec, i);
        assert(*value == arr[i]);
    }
    assert(*(char *)FuVec_first(vec) == arr[0]);
    assert(*(char *)FuVec_last(vec) == arr[arr_len - 1]);

    char res;
    FuVec_remove(vec, 1, &res);
    assert(res == arr[1]);
    assert(FuVec_len(vec) == arr_len - 1);
    assert(*(char *)FuVec_get(vec, 0) == arr[0]);
    assert(*(char *)FuVec_get(vec, 1) == arr[2]);
    assert(*(char *)FuVec_get(vec, 2) == arr[3]);

    FuVec_insert(vec, 1, &res);
    assert(res == arr[1]);
    assert(FuVec_len(vec) == arr_len);
    assert(*(char *)FuVec_get(vec, 0) == arr[0]);
    assert(*(char *)FuVec_get(vec, 1) == arr[1]);
    assert(*(char *)FuVec_get(vec, 2) == arr[2]);

    FuVec *vec2 = FuVec_copy(vec);
    assert(vec2->len == vec->len);
    assert(vec2->cap == vec->cap);
    assert(vec2->item_size == vec->item_size);
    for (i = 0; i < vec->len; i++) {
        assert(*(char *)FuVec_get(vec2, i) == *(char *)FuVec_get(vec, i));
    }

    FuVec_append(vec, vec2);
    assert(vec->len == arr_len * 2);
    assert(vec2->len == 0);
    for (i = 0; i < vec->len; i++) {
        assert(*(char *)FuVec_get(vec, i) == arr[i % arr_len]);
    }

    err = FuVec_pop(vec, &res);
    assert(!err);
    assert(res == arr[arr_len - 1]);

    FuVec_clear(vec);
    assert(FuVec_is_empty(vec));

    FuVec_drop(vec);
    FuVec_drop(vec2);
}

void test_vec_init(void) {
    fu_size_t i;

    char arr[] = {'a', 'b', 'c', 'd', 'e'};
    fu_size_t arr_len = sizeof(arr) / sizeof(arr[0]);
    FuVec vec;
    FuVec_init(&vec, sizeof(arr[0]));

    assert(FuVec_is_empty(&vec) == FU_TRUE);

    for (i = 0; i < arr_len; i++) {
        FuVec_push(&vec, arr + i);
        assert(vec.len == i + 1);
    }

    assert(FuVec_capacity(&vec) == 8);
    for (i = 0; i < arr_len; i++) {
        char *value = FuVec_get(&vec, i);
        assert(*value == arr[i]);
    }
    assert(*(char *)FuVec_first(&vec) == arr[0]);
    assert(*(char *)FuVec_last(&vec) == arr[arr_len - 1]);

    FuVec_deinit(&vec);
}

void test_vec_ptr(void) {
    int err;
    fu_size_t i;

    char *arr[] = {"a", "b", "c", "d", "e"};
    fu_size_t arr_len = 5;

    FuVec *vec = FuVec_new(sizeof(char *));
    assert(FuVec_is_empty(vec) == FU_TRUE);

    for (i = 0; i < arr_len; i++) {
        FuVec_push_ptr(vec, arr[i]);
        assert(vec->len == i + 1);
    }

    for (i = 0; i < arr_len; i++) {
        char *value = FuVec_get_ptr(vec, i);
        assert(*value == arr[i][0]);
    }

    assert(*(char *)FuVec_first_ptr(vec) == arr[0][0]);
    assert(*(char *)FuVec_last_ptr(vec) == arr[arr_len - 1][0]);

    char *res;
    FuVec_remove_ptr(vec, 1, (void **)&res);
    assert(*res == arr[1][0]);
    assert(FuVec_len(vec) == arr_len - 1);
    assert(*(char *)FuVec_get_ptr(vec, 0) == arr[0][0]);
    assert(*(char *)FuVec_get_ptr(vec, 1) == arr[2][0]);
    assert(*(char *)FuVec_get_ptr(vec, 2) == arr[3][0]);

    FuVec_insert(vec, 1, &res);
    assert(res == arr[1]);
    assert(FuVec_len(vec) == arr_len);
    assert(*(char *)FuVec_get_ptr(vec, 0) == arr[0][0]);
    assert(*(char *)FuVec_get_ptr(vec, 1) == arr[1][0]);
    assert(*(char *)FuVec_get_ptr(vec, 2) == arr[2][0]);

    FuVec *vec2 = FuVec_copy(vec);
    assert(vec2->len == vec->len);
    assert(vec2->cap == vec->cap);
    assert(vec2->item_size == vec->item_size);
    for (i = 0; i < vec->len; i++) {
        assert(*(char *)FuVec_get_ptr(vec2, i) == *(char *)FuVec_get_ptr(vec, i));
    }

    FuVec_append(vec, vec2);
    assert(vec->len == arr_len * 2);
    assert(vec2->len == 0);
    for (i = 0; i < vec->len; i++) {
        assert(*(char *)FuVec_get_ptr(vec, i) == arr[i % arr_len][0]);
    }

    err = FuVec_pop(vec, &res);
    assert(!err);
    assert(*res == arr[arr_len - 1][0]);

    FuVec_clear(vec);
    assert(FuVec_is_empty(vec));

    FuVec_drop(vec);
    FuVec_drop(vec2);
}

int main(void) {
    test_vec();
    test_vec_init();
    test_vec_ptr();
    return 0;
}
