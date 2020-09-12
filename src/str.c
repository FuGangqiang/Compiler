#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "alloc.h"
#include "log.h"
#include "str.h"

FuStr *FuStr_new() {
    FuStr *str = (FuStr *)FuMem_alloc(sizeof(FuStr));
    FuStr_init(str);
    return str;
}

FuStr *FuStr_with_capacity(fu_size_t capacity) {
    FuStr *str = FuStr_new();
    FuStr_reserve(str, capacity);
    return str;
}

void FuStr_init(FuStr *str) {
    str->len = 0;
    str->cap = 0;
    str->chars = NULL;
}

void FuStr_deinit(FuStr *str) {
    FuMem_free(str->chars);
}

void FuStr_drop(FuStr *str) {
    if (!str) {
        return;
    }
    FuStr_deinit(str);
    FuMem_free(str);
}

void FuStr_reserve(FuStr *str, fu_size_t additional) {
    fu_size_t new_cap = str->len + additional;
    if (str->cap > new_cap) {
        return;
    }
    FuChar *chars = (FuChar *)FuMem_alloc(sizeof(FuChar) * new_cap);
    memcpy(chars, str->chars, str->len * sizeof(FuChar));
    FuMem_free(str->chars);
    str->cap = new_cap;
    str->chars = chars;
}

void FuStr_make_room(FuStr *str) {
    if (str->cap > str->len) {
        return;
    }
    fu_size_t additional = str->cap > 0 ? str->cap : 4;
    FuStr_reserve(str, additional);
}

void FuStr_shrink_to_fit(FuStr *str) {
    if (str->cap == str->len) {
        return;
    }
    fu_size_t new_cap = str->len;
    FuChar *chars = (FuChar *)FuMem_alloc(sizeof(FuChar) * new_cap);
    memcpy(chars, str->chars, str->len * sizeof(FuChar));
    FuMem_free(str->chars);
    str->cap = new_cap;
    str->chars = chars;
}

FuStr *FuStr_clone(FuStr *str) {
    FuStr *new = FuStr_with_capacity(str->len);
    memcpy(new->chars, str->chars, str->len * sizeof(FuChar));
    new->len = str->len;
    new->cap = str->len;
    return new;
}

FuStr *FuStr_from_slice(FuStr *str, fu_size_t start, fu_size_t len) {
    fu_size_t end = FuStr_len(str);
    len = end - start >= len ? len : end - start;
    FuStr *new = FuStr_with_capacity(len);
    memcpy(new->chars, str->chars + start, len * sizeof(FuChar));
    new->len = len;
    return new;
}

FuStr *FuStr_from_utf8_cstr(char *cstr) {
    fu_size_t len = strlen(cstr);
    FuStr *str = FuStr_with_capacity(len);
    FuStr_push_utf8(str, cstr, len);
    return str;
}

void FuStr_push(FuStr *str, FuChar c) {
    FuStr_make_room(str);
    str->chars[str->len] = c;
    str->len++;
}

void FuStr_push_utf8(FuStr *str, char *s, int len) {
    FuStr_reserve(str, len);

    FuChar fc;
    char *sp = s;
    int consume = 0;
    while (len > 0) {
        consume = FuChar_from_utf8(&fc, sp, len);
        if (consume > 0) {
            sp += consume;
            len -= consume;
        } else {
            break;
        }
        FuStr_push(str, fc);
    }
}

void FuStr_push_utf8_cstr(FuStr *str, char *s) {
    fu_size_t len = strlen(s);
    FuStr_reserve(str, len);

    FuChar fc;
    char *sp = s;
    int consume = 0;
    while (len > 0) {
        consume = FuChar_from_utf8(&fc, sp, len);
        if (consume > 0) {
            sp += consume;
            len -= consume;
        } else {
            break;
        }
        FuStr_push(str, fc);
    }
}

void FuStr_push_utf8_format(FuStr *str, const char *format, ...) {
    va_list ap;
    va_start(ap, format);
    FuStr_push_utf8_format_v(str, format, ap);
    va_end(ap);
}

void FuStr_push_utf8_format_v(FuStr *str, const char *format, va_list params) {
    char buf[4096];
    int count = vsprintf(buf, format, params);
    if (count >= 4096) {
        count = 4095;
    }
    buf[count] = '\0';
    FuStr_push_utf8_cstr(str, buf);
}

void FuStr_append(FuStr *str, FuStr *other) {
    fu_size_t i;
    fu_size_t other_len = FuStr_len(other);
    for (i = 0; i < other_len; i++) {
        FuChar fc = FuStr_get_char(other, i);
        FuStr_push(str, fc);
    }
    FuStr_drop(other);
}

fu_size_t FuStr_len(FuStr *str) {
    return str->len;
}

fu_size_t FuStr_capacity(FuStr *str) {
    return str->cap;
}

fu_bool_t FuStr_is_empty(FuStr *str) {
    return str->len == 0;
}

FuChar FuStr_first_char(FuStr *str) {
    assert(str->len > 0);
    return str->chars[0];
}

FuChar FuStr_last_char(FuStr *str) {
    assert(str->len > 0);
    return str->chars[str->len - 1];
}

FuChar FuStr_get_char(FuStr *str, fu_size_t i) {
    assert(str->len > i);
    return str->chars[i];
}

void FuStr_set_char(FuStr *str, fu_size_t i, FuChar fc) {
    assert(str->len > i);
    str->chars[i] = fc;
}

int FuStr_cmp(FuStr **str1, FuStr **str2) {
    if (*str1 == *str2) {
        return 0;
    }
    if (*str1 == NULL) {
        return -1;
    }
    if (*str2 == NULL) {
        return 1;
    }

    fu_size_t len1 = FuStr_len(*str1);
    fu_size_t len2 = FuStr_len(*str2);
    fu_size_t i = 0;
    while (i != len1 && i != len2) {
        FuChar fc1 = FuStr_get_char(*str1, i);
        FuChar fc2 = FuStr_get_char(*str2, i);
        if (fc1 > fc2) {
            return 1;
        }
        if (fc1 < fc2) {
            return -1;
        }
        i++;
    };
    if (i == len1 && i == len2) {
        return 0;
    }
    if (i == len1) {
        return -1;
    }
    return 1;
}

fu_bool_t FuStr_eq(FuStr **str1, FuStr **str2) {
    return FuStr_cmp(str1, str2) == 0;
}

fu_size_t FuStr_hash(FuStr **str) {
    if ((*str)->len == 0) {
        return 0;
    }
    return hash_bytes((fu_uint8_t *)(*str)->chars, (*str)->len * 4);
}

fu_bool_t FuStr_eq_cstr(FuStr *str, char *cstr) {
    FuStr *new = FuStr_from_utf8_cstr(cstr);
    fu_bool_t res = FuStr_eq(&str, &new);
    FuStr_drop(new);
    return res;
}

fu_bool_t FuStr_starts_with(FuStr *str, FuStr *pat) {
    fu_size_t str_len = FuStr_len(str);
    fu_size_t pat_len = FuStr_len(pat);
    if (pat_len > str_len) {
        return FU_FALSE;
    }
    fu_size_t i;
    for (i = 0; i < pat_len; i++) {
        FuChar fc0 = FuStr_get_char(str, i);
        FuChar fc1 = FuStr_get_char(pat, i);
        if (fc0 != fc1) {
            return FU_FALSE;
        }
    }
    return FU_TRUE;
}

fu_bool_t FuStr_ends_with(FuStr *str, FuStr *pat) {
    fu_size_t str_len = FuStr_len(str);
    fu_size_t pat_len = FuStr_len(pat);
    if (pat_len > str_len) {
        return FU_FALSE;
    }
    fu_size_t last_idx = pat_len - 1;
    fu_size_t i;
    for (i = 0; i < pat_len; i++) {
        FuChar fc0 = FuStr_get_char(str, last_idx - i);
        FuChar fc1 = FuStr_get_char(pat, last_idx - i);
        if (fc0 != fc1) {
            return FU_FALSE;
        }
    }
    return FU_TRUE;
}

void FuStr_to_utf8(FuStr *str, char *buf, fu_size_t len) {
    fu_size_t str_len = FuStr_len(str);
    fu_size_t consumed = 0;
    fu_size_t i;
    for (i = 0; i < str_len; i++) {
        FuChar fc = FuStr_get_char(str, i);
        consumed += FuChar_to_utf8(buf + consumed, len - consumed, fc);
    }
    if (consumed >= len) {
        consumed = len - 1;
    }
    buf[consumed] = 0;
}

void FuStr_read_file(FuStr *str, FuStr *fpath) {
    char path[4096];
    FuStr_to_utf8(fpath, path, 4096);
    FILE *f = fopen(path, "r");
    if (!f) {
        FATAL1(NULL, "can not open file: `%s`", path);
    }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    rewind(f);
    FuStr_reserve(str, fsize);
    char *buf = (char *)FuMem_alloc(fsize);
    fread(buf, 1, fsize, f);
    FuStr_push_utf8(str, buf, fsize);
    FuMem_free(buf);
    fclose(f);
}

int FuStr_print(FILE *out, FuStr *str) {
    char buf[8];
    fu_size_t n;
    fu_size_t acc = 0;
    fu_size_t i;
    for (i = 0; i < str->len; i++) {
        n = FuChar_to_utf8(buf, 8, str->chars[i]);
        buf[n] = '\0';
        acc += fprintf(out, "%s", buf);
    }
    return acc;
}

int FuStr_print_slice(FILE *out, FuStr *str, fu_size_t start, fu_size_t len) {
    fu_size_t end = FuStr_len(str);
    len = end - start >= len ? len : end - start;

    char buf[8];
    fu_size_t n;
    fu_size_t acc = 0;
    fu_size_t i;
    for (i = start; i < start + len; i++) {
        n = FuChar_to_utf8(buf, 8, str->chars[i]);
        buf[n] = '\0';
        acc += fprintf(out, "%s", buf);
    }
    return acc;
}

void FuStr_clear(FuStr *str) {
    str->len = 0;
}
