#include <assert.h>
#include <string.h>

#include "alloc.h"
#include "log.h"

void *FuMem_alloc(fu_size_t size) {
    void *ptr = malloc(size);
    if (!ptr) {
        FATAL(NULL, "memory exhausted");
    }
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
    free(ptr);
}
