#include <assert.h>
#include <stdio.h>

#include "bitset.h"

void test_bitset(void) {
    fu_size_t i;

    FuBitSet *bs = FuBitSet_new();
    assert(FuBitSet_count(bs) == 0);

    for (i = 0; i < 20; i++) {
        if (i % 2 == 0) {
            FuBitSet_insert(bs, i);
        }
    }
    assert(FuBitSet_count(bs) == 20 / 2);

    for (i = 0; i < 20; i++) {
        if (i % 2 == 0) {
            assert(FuBitSet_contains(bs, i) == FU_TRUE);
        } else {
            assert(FuBitSet_contains(bs, i) == FU_FALSE);
        }
    }

    FuBitSet *cloned = FuBitSet_clone(bs);
    assert(FuBitSet_eq(bs, cloned) == FU_TRUE);

    FuBitSet_clear(bs);
    assert(FuBitSet_count(bs) == 0);

    FuBitSet_drop(bs);
    FuBitSet_drop(cloned);
}

void test_bitset_ops() {
    fu_size_t i;

    FuBitSet *bs1 = FuBitSet_new();
    FuBitSet *bs2 = FuBitSet_new();
    FuBitSet *bs3 = FuBitSet_new();
    FuBitSet *bs4 = FuBitSet_new();

    for (i = 0; i < 20; i++) {
        if (i % 2) {
            FuBitSet_insert(bs1, i);
            FuBitSet_insert(bs2, i);
            FuBitSet_insert(bs3, i);
        } else {
            FuBitSet_insert(bs4, i);
        }
    }
    FuBitSet_insert(bs4, 1);
    FuBitSet_remove(bs1, 3);
    FuBitSet_remove(bs2, 3);
    FuBitSet_remove(bs3, 3);

    assert(FuBitSet_count(bs1) == 9);
    assert(FuBitSet_count(bs2) == 9);
    assert(FuBitSet_count(bs3) == 9);
    assert(FuBitSet_count(bs4) == 11);

    FuBitSet_union(bs1, bs4);
    assert(FuBitSet_count(bs1) == 19);

    FuBitSet_intersect(bs2, bs4);
    assert(FuBitSet_count(bs2) == 1);

    FuBitSet_diff(bs3, bs4);
    assert(FuBitSet_count(bs3) == 8);

    FuBitSet_drop(bs1);
    FuBitSet_drop(bs2);
    FuBitSet_drop(bs3);
    FuBitSet_drop(bs4);
}

int main(void) {
    test_bitset();
    test_bitset_ops();
    return 0;
}
