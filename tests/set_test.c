#include <assert.h>
#include <stdio.h>

#include "set.h"

fu_size_t int_hash(int *item) {
    return *item;
}

fu_bool_t int_eq(int *item1, int *item2) {
    if (*item1 == *item2) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_size_t int_ptr_hash(int **item) {
    return **item;
}

fu_bool_t int_ptr_eq(int **item1, int **item2) {
    if (**item1 == **item2) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

void test_set(void) {
    fu_size_t i;

    int keys[] = {0, 1, 2, 3, 4};

    fu_size_t keys_len = sizeof(keys) / sizeof(keys[0]);

    FuSet *set = FuSet_new(sizeof(keys[0]), (FuEqFn)int_eq, (FuHashFn)int_hash);

    assert(set);
    assert(FuSet_is_empty(set) == FU_TRUE);
    fu_size_t idx;
    for (i = 0; i < keys_len; i++) {
        FuSet_add(set, &keys[i], &idx);
        assert(idx == i);
    }

    assert(FuSet_len(set) == keys_len);
    assert(FuSet_capacity(set) == 8);
    for (i = 0; i < keys_len; i++) {
        assert(FuSet_contains(set, &keys[i], &idx));
        assert(idx == i);
    }

    int key = 3;
    FuSet_add(set, &key, &idx);
    assert(idx == key);

    FuSet *new = FuSet_copy(set);
    assert(FuSet_len(new) == keys_len);
    for (i = 0; i < keys_len; i++) {
        assert(*(int *)FuSet_key_at(new, i) == keys[i]);
    }
    FuSet_drop(new);

    FuSet_remove(set, &keys[2]);
    assert(FuSet_len(set) == keys_len - 1);
    assert(!FuSet_contains(set, &keys[2], NULL));

    new = FuSet_new(sizeof(keys[0]), (FuEqFn)int_eq, (FuHashFn)int_hash);
    for (i = 0; i < keys_len; i++) {
        int key = keys[i] + 1;
        FuSet_add(new, &key, NULL);
    }
    FuSet_merge(set, new);
    assert(FuSet_len(set) == keys_len + 1);

    FuSet_clear(set);
    assert(FuSet_is_empty(set));

    FuSet_drop(set);
    FuSet_drop(new);
}

void test_set_ptr(void) {
    fu_size_t i;

    fu_size_t len = 5;
    int keys[5][1] = {{0}, {1}, {2}, {3}, {4}};

    FuSet *set = FuSet_new(sizeof(&keys[0][0]), (FuEqFn)int_ptr_eq, (FuHashFn)int_ptr_hash);

    assert(set);
    assert(FuSet_is_empty(set) == FU_TRUE);
    fu_size_t idx;
    for (i = 0; i < len; i++) {
        FuSet_add_ptr(set, &keys[i][0], &idx);
        assert(idx == i);
    }

    assert(FuSet_len(set) == len);
    assert(FuSet_capacity(set) == 8);
    int key;
    for (i = 0; i < len; i++) {
        key = keys[i][0];
        assert(FuSet_contains_ptr(set, &key, NULL));
    }

    key = 3;
    FuSet_add_ptr(set, &key, &idx);
    assert(idx == key);

    FuSet *new = FuSet_copy(set);
    assert(FuSet_len(new) == len);
    for (i = 0; i < len; i++) {
        assert(*(int *)FuSet_key_ptr_at(new, i) == keys[i][0]);
    }
    FuSet_drop(new);

    FuSet_remove_ptr(set, &keys[2][0]);
    assert(FuSet_len(set) == len - 1);
    assert(!FuSet_contains_ptr(set, &keys[2][0], NULL));

    int keys2[5][1] = {{1}, {2}, {3}, {4}, {5}};
    new = FuSet_new(sizeof(&keys2[0][0]), (FuEqFn)int_ptr_eq, (FuHashFn)int_ptr_hash);
    for (i = 0; i < len; i++) {
        FuSet_add_ptr(new, &keys2[i][0], NULL);
    }

    FuSet_merge_ptr(set, new);
    assert(FuSet_len(set) == len + 1);

    FuSet_clear(set);
    assert(FuSet_is_empty(set));

    FuSet_drop(set);
    FuSet_drop(new);
}

int main(void) {
    test_set();
    test_set_ptr();
    return 0;
}
