#include <assert.h>
#include <string.h>

#include "alloc.h"
#include "log.h"

FuHeap *CURRENT_HEAP_LIST;
static FuHeap *HEAP_FREE_LIST;

static void FuHeap_reset(FuHeap *heap) {
    heap->n_freeze = 0;
    heap->avail = (char *)((FuHeapHeader *)heap + 1);
}

FuHeap *FuHeap_new(fu_size_t extra) {
    size_t n = sizeof(FuHeapHeader) + extra + HEAP_ALIGN(HEAP_MIN_SIZE, sizeof(FuHeapAlign));
    FuHeap *heap = malloc(n);
    if (!heap) {
        FATAL(NULL, "memory exhausted");
    }
    heap->next = NULL;
    heap->size = n;
    heap->limit = (char *)heap + n;
    FuHeap_reset(heap);
    return heap;
}

void FuHeap_drop(FuHeap *heap) {
    while (heap) {
        FuHeap *temp = heap;
        heap = heap->next;
        FuHeapFreeList_push(temp);
    }
}

fu_size_t FuHeap_len(FuHeap *heap) {
    fu_size_t len = 0;
    FuHeap *h = heap;
    while (h) {
        len++;
        h = h->next;
    }
    return len;
}

fu_size_t FuHeap_free_size(FuHeap *heap) {
    return heap->limit - heap->avail;
}

fu_size_t FuHeapList_len() {
    return FuHeap_len(CURRENT_HEAP_LIST);
}

void FuHeapList_init(fu_bool_t use_heap) {
    CURRENT_HEAP_LIST = FuHeap_new(0);
}

void FuHeapList_drop() {
    FuHeap_drop(CURRENT_HEAP_LIST);
    FuHeapFreeList_drop();
}

FuHeap *FuHeapList_save_point() {
    if (!CURRENT_HEAP_LIST) {
        return NULL;
    }
    FuHeap *heap = CURRENT_HEAP_LIST;
    heap->n_freeze += 1;
    return heap;
}

static fu_bool_t FuHeapList_is_in_use(FuHeap *heap) {
    FuHeap *curr = CURRENT_HEAP_LIST;
    while (curr) {
        if (curr == heap) {
            return FU_TRUE;
        }
        curr = curr->next;
    }
    return FU_FALSE;
}

void FuHeapList_roll_back(FuHeap *heap) {
    assert(FuHeapList_is_in_use(heap));
    FuHeap *curr = CURRENT_HEAP_LIST;
    while (curr && curr != heap) {
        FuHeap *temp = curr;
        curr = curr->next;
        FuHeapFreeList_push(temp);
    }
    assert(curr && curr->n_freeze);
    curr->n_freeze -= 1;
    CURRENT_HEAP_LIST = curr;
}

fu_size_t FuHeapFreeList_len() {
    return FuHeap_len(HEAP_FREE_LIST);
}

void FuHeapFreeList_push(FuHeap *heap) {
    if (!heap) {
        return;
    }
    FuHeap_reset(heap);
    heap->next = HEAP_FREE_LIST;
    HEAP_FREE_LIST = heap;
}

FuHeap *FuHeapFreeList_pop(fu_size_t m) {
    FuHeap *prev = NULL;
    FuHeap *heap = HEAP_FREE_LIST;
    while (heap != NULL) {
        if (m <= FuHeap_free_size(heap)) {
            /* find and remove from list */
            if (prev) {
                prev->next = heap->next;
            } else {
                HEAP_FREE_LIST = NULL;
            }
            break;
        }
        prev = heap;
        heap = heap->next;
    }
    return heap;
}

void FuHeapFreeList_drop() {
    FuHeap *heap;
    while (HEAP_FREE_LIST) {
        heap = HEAP_FREE_LIST;
        HEAP_FREE_LIST = HEAP_FREE_LIST->next;
        free(heap);
    }
}

void *FuMem_alloc(fu_size_t size) {
    void *ptr;
    if (!CURRENT_HEAP_LIST) {
        ptr = malloc(size);
        if (!ptr) {
            FATAL(NULL, "memory exhausted");
        }
        return ptr;
    }

    size_t m = HEAP_ALIGN(size, sizeof(FuHeapAlign));
    /* find mem in heap */
    FuHeap *heap = CURRENT_HEAP_LIST;
    while (heap && !heap->n_freeze && m > FuHeap_free_size(heap)) {
        heap = heap->next;
    }
    if (heap && !heap->n_freeze) {
        goto end;
    }

    heap = FuHeapFreeList_pop(m);
    if (!heap) {
        heap = FuHeap_new(size);
    }

    /* insert the new heap to the CURRENT_HEAP_LIST */
    heap->next = CURRENT_HEAP_LIST;
    CURRENT_HEAP_LIST = heap;

end:
    ptr = heap->avail;
    heap->avail += m;
    return ptr;
}

void *FuMem_zalloc(fu_size_t size) {
    void *ptr = FuMem_alloc(size);
    memset(ptr, 0, size);
    return ptr;
}

void FuMem_free(void *ptr) {
    if (!ptr) {
        return;
    }
    if (!CURRENT_HEAP_LIST) {
        free(ptr);
    }
    /* do nothing if CURRENT_HEAP_LIST is not NULL */
}
