#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"
#include "error.h"

FuMemHeap *CURRENT_HEAP;
static FuMemHeap *HEAP_FREE_LIST;

FuMemHeap *FuMemHeap_new(fu_size_t extra) {
    assert(extra > 0);

    size_t n = sizeof(FuMemHeapHeader) + extra + HEAP_ALIGN(HEAP_MIN_SIZE, sizeof(FuMemHeapAlign));
    FuMemHeap *heap = malloc(n);
    if (!heap) {
        FATAL(NULL, "memory exhausted");
    }
    heap->next = NULL;
    heap->limit = (char *)heap + n;
    heap->avail = (char *)((FuMemHeapHeader *)heap + 1);
    return heap;
}

void FuMemHeap_drop(FuMemHeap *heap) {
    FuMemHeap *head = heap;
    FuMemHeap *prev;
    do {
        heap->avail = (char *)((FuMemHeapHeader *)heap + 1);
        prev = heap;
        heap = heap->next;
    } while (heap);
    prev->next = HEAP_FREE_LIST;
    HEAP_FREE_LIST = head;
}

fu_size_t FuMemHeap_len(FuMemHeap *heap) {
    fu_size_t len = 0;
    FuMemHeap *h = heap;
    while (h) {
        len++;
        h = h->next;
    }
    return len;
}

fu_size_t FuMemHeapFreeList_len() {
    return FuMemHeap_len(HEAP_FREE_LIST);
}

void FuMemHeapFreeList_drop() {
    FuMemHeap *heap;
    while (HEAP_FREE_LIST) {
        heap = HEAP_FREE_LIST;
        HEAP_FREE_LIST = HEAP_FREE_LIST->next;
        WITH_MEM_HEAP(NULL) {
            FuMem_free(heap);
        }
        END_WITH
    }
}

void *FuMem_malloc(fu_size_t size) {
    void *ptr;
    if (!CURRENT_HEAP) {
        ptr = malloc(size);
        if (!ptr) {
            FATAL(NULL, "memory exhausted");
        }
        return ptr;
    }

    FuMemHeap *heap;
    size_t m = HEAP_ALIGN(size, sizeof(FuMemHeapAlign));

    /* find mem in heap */
    heap = CURRENT_HEAP;
    while (heap && m > heap->limit - heap->avail) {
        heap = heap->next;
    }
    if (heap) {
        goto end;
    }

    /* find mem in free heap list */
    FuMemHeap *prev_free_heap = NULL;
    FuMemHeap *curr_free_heap = HEAP_FREE_LIST;
    while ((heap = curr_free_heap) != NULL) {
        if (m <= heap->limit - heap->avail) {
            /* remove the found heap from HEAP_FREE_LIST */
            if (prev_free_heap) {
                prev_free_heap->next = curr_free_heap->next;
            } else if (HEAP_FREE_LIST) {
                HEAP_FREE_LIST = NULL;
            }
            break;
        }
        prev_free_heap = curr_free_heap;
        curr_free_heap = curr_free_heap->next;
    }

    /* not find in free heap list, alloc new heap */
    if (!heap) {
        heap = FuMemHeap_new(size);
    }

    /* insert the new heap to the CURRENT_HEAP */
    heap->next = CURRENT_HEAP->next;
    CURRENT_HEAP->next = heap;

end:
    ptr = heap->avail;
    heap->avail += m;
    return ptr;
}

void *FuMem_zalloc(fu_size_t size) {
    void *ptr = FuMem_malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

void FuMem_free(void *ptr) {
    if (!CURRENT_HEAP) {
        free(ptr);
    }
    /* do nothing if CURRENT_HEAP is not NULL */
}
