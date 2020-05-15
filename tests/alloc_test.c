#include <assert.h>

#include "alloc.h"
#include "str.h"

void test_heap_free_list(void) {
    int *ptr;
    FuMemHeap *heap;

    assert(FuMemHeapFreeList_len() == 0);

    heap = FuMemHeap_new(HEAP_MIN_SIZE * 2);
    assert(FuMemHeap_len(heap) == 1);
    FuMemHeap_drop(heap);
    assert(FuMemHeapFreeList_len() == 1);

    heap = FuMemHeap_new(1);
    assert(FuMemHeap_len(heap) == 1);
    assert(FuMemHeapFreeList_len() == 1);

    WITH_MEM_HEAP(heap) {
        ptr = FuMem_malloc(HEAP_MIN_SIZE * 2);
        *ptr += 1;
    }
    END_WITH

    assert(FuMemHeap_len(heap) == 2);
    assert(FuMemHeapFreeList_len() == 0);
    FuMemHeap_drop(heap);

    FuMemHeapFreeList_drop();
}

void test_heap(void) {
    FuMemHeap *heap;

    assert(FuMemHeapFreeList_len() == 0);

    heap = FuMemHeap_new(1);

#define LEN 10
    int i;
    size_t *ptrs[LEN];
    WITH_MEM_HEAP(heap) {
        for (i = 0; i < LEN; i++) {
            ptrs[i] = FuMem_malloc(sizeof(size_t));
        }
        FuStr *str = FuStr_from_utf8_cstr("hello");
        FuStr_push_utf8_cstr(str, " world");
    }
    END_WITH
    assert(FuMemHeap_len(heap) == 1);
    for (i = 1; i < LEN; i++) {
        assert((char *)ptrs[i] - (char *)ptrs[i - 1] == sizeof(size_t));
    }

    FuMemHeap_drop(heap);
    FuMemHeapFreeList_drop();
}

int main(void) {
    test_heap();
    test_heap_free_list();
    return 0;
}