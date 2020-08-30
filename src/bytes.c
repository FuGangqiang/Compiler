#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "alloc.h"
#include "bytes.h"
#include "log.h"

FuBytes *FuBytes_new() {
    FuBytes *bytes = (FuBytes *)FuMem_alloc(sizeof(FuBytes));
    FuBytes_init(bytes);
    return bytes;
}

FuBytes *FuBytes_with_capacity(fu_size_t capacity) {
    FuBytes *bytes = FuBytes_new();
    FuBytes_reserve(bytes, capacity);
    return bytes;
}

void FuBytes_init(FuBytes *bytes) {
    bytes->len = 0;
    bytes->cap = 0;
    bytes->chars = NULL;
}

void FuBytes_deinit(FuBytes *bytes) {
    FuMem_free(bytes->chars);
}

void FuBytes_drop(FuBytes *bytes) {
    if (!bytes) {
        return;
    }
    FuBytes_deinit(bytes);
    FuMem_free(bytes);
}

void FuBytes_reserve(FuBytes *bytes, fu_size_t additional) {
    fu_size_t new_cap = bytes->len + additional;
    if (bytes->cap > new_cap) {
        return;
    }
    char *chars = (char *)FuMem_alloc(sizeof(char) * new_cap);
    memcpy(chars, bytes->chars, bytes->len * sizeof(char));
    FuMem_free(bytes->chars);
    bytes->cap = new_cap;
    bytes->chars = chars;
}

void FuBytes_make_room(FuBytes *bytes) {
    if (bytes->cap > bytes->len) {
        return;
    }
    fu_size_t additional = bytes->cap > 0 ? bytes->cap : 4;
    FuBytes_reserve(bytes, additional);
}

void FuBytes_shrink_to_fit(FuBytes *bytes) {
    if (bytes->cap == bytes->len) {
        return;
    }
    fu_size_t new_cap = bytes->len;
    char *chars = (char *)FuMem_alloc(sizeof(char) * new_cap);
    memcpy(chars, bytes->chars, bytes->len * sizeof(char));
    FuMem_free(bytes->chars);
    bytes->cap = new_cap;
    bytes->chars = chars;
}

FuBytes *FuBytes_clone(FuBytes *bytes) {
    FuBytes *new = FuBytes_with_capacity(bytes->len);
    memcpy(new->chars, bytes->chars, bytes->len * sizeof(char));
    new->len = bytes->len;
    new->cap = bytes->len;
    return new;
}

FuBytes *FuBytes_from_slice(FuBytes *bytes, fu_size_t start, fu_size_t len) {
    fu_size_t end = FuBytes_len(bytes);
    len = end - start >= len ? len : end - start;
    FuBytes *new = FuBytes_with_capacity(len);
    memcpy(new->chars, bytes->chars + start, len * sizeof(char));
    new->len = len;
    return new;
}

FuBytes *FuBytes_from_cstr(char *cstr) {
    fu_size_t len = strlen(cstr);
    FuBytes *bytes = FuBytes_with_capacity(len);
    FuBytes_push_bytes(bytes, cstr, len);
    return bytes;
}

FuStr *FuBytes_to_str(FuBytes *bytes) {
    FuStr *str = FuStr_new();
    fu_size_t len = FuBytes_len(bytes);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        char b = FuBytes_get_byte(bytes, i);
        FuStr_push(str, b);
    }
    FuBytes_drop(bytes);
    return str;
}

fu_size_t FuBytes_len(FuBytes *bytes) {
    return bytes->len;
}

fu_size_t FuBytes_capacity(FuBytes *bytes) {
    return bytes->cap;
}

fu_bool_t FuBytes_is_empty(FuBytes *bytes) {
    return bytes->len == 0;
}

char FuBytes_get_byte(FuBytes *bytes, fu_size_t i) {
    assert(bytes->len > i);
    return bytes->chars[i];
}

void FuBytes_set_byte(FuBytes *bytes, fu_size_t i, char c) {
    assert(bytes->len > i);
    bytes->chars[i] = c;
}

int FuBytes_cmp(FuBytes **bytes1, FuBytes **bytes2) {
    if (*bytes1 == *bytes2) {
        return 0;
    }
    if (*bytes1 == NULL) {
        return -1;
    }
    if (*bytes2 == NULL) {
        return 1;
    }

    fu_size_t len1 = FuBytes_len(*bytes1);
    fu_size_t len2 = FuBytes_len(*bytes2);
    fu_size_t i = 0;
    while (i != len1 && i != len2) {
        char c1 = FuBytes_get_byte(*bytes1, i);
        char c2 = FuBytes_get_byte(*bytes2, i);
        if (c1 > c2) {
            return 1;
        }
        if (c1 < c2) {
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

fu_bool_t FuBytes_eq(FuBytes **bytes1, FuBytes **bytes2) {
    return FuBytes_cmp(bytes1, bytes2) == 0;
}

fu_size_t FuBytes_hash(FuBytes **bytes) {
    if ((*bytes)->len == 0) {
        return 0;
    }
    return hash_bytes((fu_uint8_t *)(*bytes)->chars, (*bytes)->len);
}

void FuBytes_push(FuBytes *bytes, char c) {
    FuBytes_make_room(bytes);
    bytes->chars[bytes->len] = c;
    bytes->len++;
}

void FuBytes_push_bytes(FuBytes *bytes, char *s, fu_size_t len) {
    FuBytes_reserve(bytes, len);
    memcpy(bytes->chars + bytes->len, s, len);
    bytes->len += len;
}

void FuBytes_push_cstr(FuBytes *bytes, char *cstr) {
    fu_size_t len = strlen(cstr);
    FuBytes_reserve(bytes, len);
    memcpy(bytes->chars + bytes->len, cstr, len);
    bytes->len += len;
}

void FuBytes_push_cstr_format(FuBytes *bytes, const char *format, ...) {
    char buf[4096];
    va_list ap;

    va_start(ap, format);
    int count = vsprintf(buf, format, ap);
    va_end(ap);

    FuBytes_push_bytes(bytes, buf, count);
}

void FuBytes_append(FuBytes *bytes, FuBytes *other) {
    fu_size_t other_len = FuBytes_len(other);
    FuBytes_reserve(bytes, other_len);
    memcpy(bytes->chars + bytes->len, other->chars, other_len);
    bytes->len += other_len;
    FuBytes_drop(other);
}

void FuBytes_read_file(FuBytes *bytes, char *fname, fu_size_t len) {
    FILE *f = fopen(fname, "r");
    if (!f) {
        FATAL1(NULL, "can not open file: %s", fname);
    }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    rewind(f);
    FuBytes_reserve(bytes, fsize);
    fread(bytes->chars, 1, fsize, f);
    fclose(f);
    bytes->len += fsize;
}

int FuBytes_print(FILE *out, FuBytes *bytes) {
    FuBytes_reserve(bytes, 1);
    FuBytes_push(bytes, 0);
    int count = fprintf(out, "%s", bytes->chars);
    bytes->len -= 1;
    return count;
}

void FuBytes_clear(FuBytes *bytes) {
    bytes->len = 0;
}
