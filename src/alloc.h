#ifndef FU_ALLOC_H
#define FU_ALLOC_H

#include "def.h"

void *FuMem_alloc(fu_size_t size);
void *FuMem_zalloc(fu_size_t size);
void FuMem_free(void *ptr);

#define FuMem_new(obj_type) (obj_type *)FuMem_zalloc(sizeof(obj_type))

#endif /* FU_ALLOC_H */
