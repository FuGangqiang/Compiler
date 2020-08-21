#ifndef FU_ALLOC_H
#define FU_ALLOC_H

#include "def.h"

#define HEAP_MIN_SIZE (10 * 1024)
#define HEAP_ALIGN(size, n) (((size) + ((n)-1)) & (~((n)-1)))

typedef struct FuMemHeap FuMemHeap;
typedef union FuMemHeapAlign FuMemHeapAlign;
typedef union FuMemHeapHeader FuMemHeapHeader;

extern FuMemHeap *CURRENT_HEAP;

union FuMemHeapAlign {
    long l;
    char *p;
    double d;
    int (*f)(void);
};

struct FuMemHeap {
    FuMemHeap *next;
    char *limit;
    char *avail;
};

union FuMemHeapHeader {
    FuMemHeap heap;
    FuMemHeapAlign align;
};

/* clang-format off */
#define WITH_MEM_HEAP(heap)                 \
{                                           \
    FuMemHeap *__heap_bak__ = CURRENT_HEAP; \
    CURRENT_HEAP = heap;                    \

#define END_WITH                 \
    CURRENT_HEAP = __heap_bak__; \
}
/* clang-format on */

FuMemHeap *FuMemHeap_new(fu_size_t extra);
void FuMemHeap_drop(FuMemHeap *heap);
fu_size_t FuMemHeap_len(FuMemHeap *heap);

fu_size_t FuMemHeapFreeList_len();
void FuMemHeapFreeList_drop();

void *FuMem_malloc(fu_size_t size);
void *FuMem_zalloc(fu_size_t size);
void FuMem_free(void *ptr);

#define FuMem_new(obj_type) (obj_type *)FuMem_zalloc(sizeof(obj_type))

#endif /* FU_ALLOC_H */
