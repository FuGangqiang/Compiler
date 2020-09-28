#include <assert.h>

#include "alloc.h"
#include "str.h"

void test_heap(void) {
    assert(FuHeapList_len() == 1);
    assert(FuHeapFreeList_len() == 0);

    int *ptr;
    ptr = FuMem_alloc(1);
    assert(ptr);
    assert(FuHeapList_len() == 1);
    assert(FuHeapFreeList_len() == 0);

    ptr = FuMem_alloc(HEAP_MIN_SIZE * 2);
    assert(ptr);
    assert(FuHeapList_len() == 2);
    assert(FuHeapFreeList_len() == 0);

    FuHeap *heap_bak = FuHeapList_save_point();
    ptr = FuMem_alloc(1);
    assert(ptr);
    assert(FuHeapList_len() == 3);
    assert(FuHeapFreeList_len() == 0);

    FuHeapList_roll_back(heap_bak);
    assert(FuHeapList_len() == 2);
    assert(FuHeapFreeList_len() == 1);
}

void test_with_macro() {
    WITH_HEAP(NULL) {
        int *p = FuMem_alloc(sizeof(int));
        FuMem_free(p);
    }
    END_WITH
}

int main(void) {
    FuHeapList_init(FU_TRUE);
    test_heap();
    test_with_macro();
    FuHeapList_drop();
    return 0;
}
