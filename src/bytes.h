#ifndef FU_BYTES_H
#define FU_BYTES_H

#include <stdarg.h>
#include <stdio.h>

#include "def.h"
#include "hash.h"
#include "str.h"

typedef struct FuBytes FuBytes;
struct FuBytes {
    fu_size_t len;
    fu_size_t cap;
    char *chars;
};

FuBytes *FuBytes_new();
FuBytes *FuBytes_with_capacity(fu_size_t capacity);
void FuBytes_init(FuBytes *bytes);
void FuBytes_deinit(FuBytes *bytes);
void FuBytes_drop(FuBytes *bytes);

void FuBytes_reserve(FuBytes *bytes, fu_size_t additional);
void FuBytes_make_room(FuBytes *bytes);
void FuBytes_shrink_to_fit(FuBytes *bytes);

FuBytes *FuBytes_clone(FuBytes *bytes);
FuBytes *FuBytes_from_slice(FuBytes *bytes, fu_size_t start, fu_size_t len);
FuBytes *FuBytes_from_cstr(char *ctr);
FuStr *FuBytes_to_str(FuBytes *bytes);

fu_size_t FuBytes_len(FuBytes *bytes);
fu_size_t FuBytes_capacity(FuBytes *bytes);
fu_bool_t FuBytes_is_empty(FuBytes *bytes);

char FuBytes_get_byte(FuBytes *bytes, fu_size_t i);
void FuBytes_set_byte(FuBytes *bytes, fu_size_t i, char c);

int FuBytes_cmp(FuBytes **bytes1, FuBytes **bytes2);
fu_bool_t FuBytes_eq(FuBytes **bytes1, FuBytes **bytes2);
fu_size_t FuBytes_hash(FuBytes **bytes);

void FuBytes_push(FuBytes *bytes, char c);
void FuBytes_push_bytes(FuBytes *bytes, char *s, fu_size_t len);
void FuBytes_push_cstr(FuBytes *bytes, char *cstr);
void FuBytes_push_cstr_format(FuBytes *bytes, const char *format, ...);

void FuBytes_append(FuBytes *bytes, FuBytes *other);

void FuBytes_read_file(FuBytes *bytes, char *fname, fu_size_t len);

int FuBytes_print(FILE *out, FuBytes *bytes);

void FuBytes_clear(FuBytes *bytes);

#endif /* FU_BYTES_H */
