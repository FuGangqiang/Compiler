#include <assert.h>
#include <stdio.h>

#include "map.h"

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

void test_map(void) {
    fu_size_t i;

    int keys[] = {0, 1, 2, 3, 4};
    char values[] = {'a', 'b', 'c', 'd', 'e'};

    fu_size_t keys_len = sizeof(keys) / sizeof(keys[0]);

    FuMap *map = FuMap_new(sizeof(keys[0]), sizeof(values[0]), (FuEqFn)int_eq, (FuHashFn)int_hash);

    assert(FuMap_is_empty(map) == FU_TRUE);
    for (i = 0; i < keys_len; i++) {
        FuMap_set(map, &keys[i], &values[i], NULL);
    }

    assert(FuMap_len(map) == keys_len);
    assert(FuMap_capacity(map) == 8);
    for (i = 0; i < keys_len; i++) {
        assert(*(char *)FuMap_get(map, &keys[i]) == values[i]);
    }

    char c = 'h';
    char old_v;
    FuMap_set(map, &keys[1], &c, &old_v);
    assert(*(char *)FuMap_get(map, &keys[1]) == c);
    assert(old_v == 'b');
    FuMap_set(map, &keys[1], &values[1], NULL);

    FuMap_remove(map, &keys[2], &old_v);
    assert(FuMap_len(map) == keys_len - 1);
    assert(FuMap_get(map, &keys[2]) == NULL);
    assert(old_v == 'c');
    FuMap_set(map, &keys[2], &values[2], NULL);

    FuMap *new = FuMap_copy(map);
    assert(FuMap_len(new) == keys_len);
    for (i = 0; i < keys_len; i++) {
        assert(*(char *)FuMap_get(new, &keys[i]) == values[i]);
    }
    FuMap_drop(new);

    new = FuMap_new(sizeof(keys[0]), sizeof(values[0]), (FuEqFn)int_eq, (FuHashFn)int_hash);
    for (i = 0; i < keys_len; i++) {
        int key = keys[i] + 1;
        char value = values[i];
        FuMap_set(new, &key, &value, NULL);
    }
    FuMap_merge(map, new);
    assert(FuMap_len(map) == keys_len + 1);
    for (i = 0; i < keys_len + 1; i++) {
        if (i == 0) {
            assert(*(char *)FuMap_get(map, &keys[i]) == values[i]);
        } else {
            int key = i;
            assert(*(char *)FuMap_get(map, &key) == values[i - 1]);
        }
    }

    FuMap_clear(map);
    assert(FuMap_is_empty(map));

    FuMap_drop(map);
    FuMap_drop(new);
}

void test_map_ptr(void) {
    fu_size_t i;

    fu_size_t len = 5;
    int keys[5][1] = {{0}, {1}, {2}, {3}, {4}};
    char *values[] = {"a", "b", "c", "d", "e"};

    FuMap *map = FuMap_new(sizeof(&keys[0][0]), sizeof(&values[0][0]), (FuEqFn)int_ptr_eq, (FuHashFn)int_ptr_hash);

    assert(FuMap_is_empty(map) == FU_TRUE);
    for (i = 0; i < len; i++) {
        FuMap_set_ptr(map, &keys[i][0], &values[i][0], NULL);
    }

    assert(FuMap_len(map) == len);
    assert(FuMap_capacity(map) == 8);
    for (i = 0; i < len; i++) {
        int key = keys[i][0];
        assert(*(char *)FuMap_get_ptr(map, &key) == values[i][0]);
    }

    char *c = "h";
    char *old_v;
    FuMap_set_ptr(map, &keys[1][0], c, (void **)&old_v);
    assert(*(char *)FuMap_get_ptr(map, &keys[1][0]) == *c);
    assert(*old_v == 'b');
    FuMap_set_ptr(map, &keys[1][0], &values[1][0], NULL);

    FuMap_remove_ptr(map, &keys[2][0], (void **)&old_v);
    assert(FuMap_len(map) == len - 1);
    assert(FuMap_get_ptr(map, &keys[2][0]) == NULL);
    assert(*old_v == 'c');
    FuMap_set_ptr(map, &keys[2][0], &values[2][0], NULL);

    FuMap *new = FuMap_copy(map);
    assert(FuMap_len(new) == len);
    for (i = 0; i < len; i++) {
        int key = keys[i][0];
        assert(*(char *)FuMap_get_ptr(new, &key) == values[i][0]);
    }

    FuMap_clear(map);
    assert(FuMap_is_empty(map));

    FuMap_drop(map);
    FuMap_drop(new);
}

void test_map_ptr2(void) {
    fu_size_t i;
    fu_size_t len = 5;
    int keys1[5][1] = {{0}, {1}, {2}, {3}, {4}};
    char *values1[] = {"a", "b", "c", "d", "e"};

    int keys2[5][1] = {{0}, {1}, {2}, {3}, {4}};
    char *values2[] = {"a", "b", "c", "d", "e"};

    FuMap *map = FuMap_new(sizeof(&keys1[0][0]), sizeof(&values1[0][0]), (FuEqFn)int_ptr_eq, (FuHashFn)int_ptr_hash);

    assert(FuMap_is_empty(map) == FU_TRUE);
    for (i = 0; i < len; i++) {
        FuMap_set_ptr(map, &keys1[i][0], &values1[i][0], NULL);
    }

    assert(FuMap_len(map) == len);
    assert(FuMap_capacity(map) == 8);
    for (i = 0; i < len; i++) {
        assert(*(char *)FuMap_get_ptr(map, &keys2[i][0]) == values2[i][0]);
    }
}

int main(void) {
    test_map();
    test_map_ptr();
    return 0;
}
