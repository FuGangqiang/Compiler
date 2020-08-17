#ifndef FU_STR_H
#define FU_STR_H

#include <stdarg.h>
#include <stdio.h>

#include "char.h"
#include "def.h"
#include "hash.h"

typedef struct FuStr FuStr;
struct FuStr {
    fu_size_t len;
    fu_size_t cap;
    FuChar *chars;
};

FuStr *FuStr_new();
FuStr *FuStr_with_capacity(fu_size_t capacity);
void FuStr_init(FuStr *str);
void FuStr_deinit(FuStr *str);
void FuStr_drop(FuStr *str);

void FuStr_reserve(FuStr *str, fu_size_t additional);
void FuStr_shrink_to_fit(FuStr *str);
void FuStr_make_room(FuStr *str);

FuStr *FuStr_clone(FuStr *str);
FuStr *FuStr_from_slice(FuStr *str, fu_size_t start, fu_size_t len);
FuStr *FuStr_from_utf8_cstr(char *cstr);

fu_size_t FuStr_len(FuStr *str);
fu_size_t FuStr_capacity(FuStr *str);
fu_bool_t FuStr_is_empty(FuStr *str);

FuChar FuStr_first_char(FuStr *str);
FuChar FuStr_last_char(FuStr *str);
FuChar FuStr_get_char(FuStr *str, fu_size_t i);
void FuStr_set_char(FuStr *str, fu_size_t i, FuChar fc);

int FuStr_cmp(FuStr **str1, FuStr **str2);
fu_bool_t FuStr_eq(FuStr **str1, FuStr **str2);
fu_size_t FuStr_hash(FuStr **str);

fu_bool_t FuStr_eq_cstr(FuStr *str, char *cstr);

void FuStr_push(FuStr *str, FuChar c);
void FuStr_push_utf8(FuStr *str, char *s, int len);
void FuStr_push_utf8_cstr(FuStr *str, char *s);
void FuStr_push_utf8_format(FuStr *str, const char *format, ...);
void FuStr_push_utf8_format_v(FuStr *str, const char *format, va_list params);

void FuStr_append(FuStr *str, FuStr *other);

void FuStr_read_file(FuStr *str, char *fpath, fu_size_t len);

int FuStr_print(FILE *out, FuStr *str);
int FuStr_print_slice(FILE *out, FuStr *str, fu_size_t start, fu_size_t len);

void FuStr_clear(FuStr *str);

#endif /* FU_STR_H */
