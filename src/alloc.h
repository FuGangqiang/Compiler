#ifndef FU_ALLOC_H
#define FU_ALLOC_H

#include "def.h"

#define HEAP_MIN_SIZE (10 * 1024)
#define HEAP_ALIGN(size, n) (((size) + ((n)-1)) & (~((n)-1)))

typedef struct FuHeap FuHeap;
typedef union FuHeapAlign FuHeapAlign;
typedef union FuHeapHeader FuHeapHeader;

extern FuHeap *CURRENT_HEAP_LIST;

union FuHeapAlign {
    long l;
    char *p;
    double d;
    int (*f)(void);
};

struct FuHeap {
    fu_size_t size;
    /* can not be alloc mem from this heap and its nexts */
    fu_size_t n_freeze;
    char *limit;
    char *avail;
    FuHeap *next;
};

union FuHeapHeader {
    FuHeap heap;
    FuHeapAlign align;
};

FuHeap *FuHeap_new(fu_size_t extra);
void FuHeap_drop(FuHeap *heap);
fu_size_t FuHeap_len(FuHeap *heap);
fu_size_t FuHeap_free_size(FuHeap *heap);

fu_size_t FuHeapList_len();
void FuHeapList_init(fu_bool_t use_heap);
void FuHeapList_drop();
FuHeap *FuHeapList_save_point();
void FuHeapList_roll_back(FuHeap *heap);

fu_size_t FuHeapFreeList_len();
void FuHeapFreeList_push(FuHeap *heap);
FuHeap *FuHeapFreeList_pop(fu_size_t m);
void FuHeapFreeList_drop();

void *FuMem_alloc(fu_size_t size);
void *FuMem_zalloc(fu_size_t size);
void FuMem_free(void *ptr);

#define FuMem_new(obj_type) (obj_type *)FuMem_zalloc(sizeof(obj_type))

/* clang-format off */
#define WITH_HEAP(heap)                            \
{                                                  \
    FuHeap *__heap_list_bak__ = CURRENT_HEAP_LIST; \
    CURRENT_HEAP_LIST = heap;                      \

#define END_WITH                           \
    CURRENT_HEAP_LIST = __heap_list_bak__; \
}
/* clang-format on */

#endif /* FU_ALLOC_H */
