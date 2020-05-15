#ifndef FU_DEF_H
#define FU_DEF_H

#include <stdint.h>
#include <stdlib.h>

enum {
    FU_FALSE = 0,
    FU_TRUE = 1,
};
typedef int fu_bool_t;
typedef int fu_err_t;

typedef unsigned char fu_uint8_t;
typedef uint32_t fu_uint32_t;
typedef uint64_t fu_uint64_t;
typedef size_t fu_size_t;
#define FU_SIZE_MAX SIZE_MAX

typedef void (*FuDropFn)(void *item);
typedef void *(*FuCloneFn)(void *item);
typedef int (*FuEqFn)(void *item1, void *item2);
typedef fu_size_t (*FuHashFn)(void *item);

#endif /* FU_DEF_H */
