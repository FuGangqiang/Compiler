#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "bytes.h"

void test_bytes_new(void) {
    FuBytes *bytes = FuBytes_new();

    assert(bytes->len == 0);
    assert(bytes->cap == 0);
    assert(bytes->chars == NULL);

    FuBytes_push(bytes, 'a');
    assert(bytes->len == 1);
    assert(bytes->chars[0] == 'a');

    FuBytes_push(bytes, 'b');
    assert(bytes->len == 2);
    assert(bytes->chars[1] == 'b');

    FuBytes_reserve(bytes, 2);
    assert(bytes->len == 2);
    assert(bytes->chars != NULL);

    FuBytes_reserve(bytes, 20);
    assert(bytes->cap == 2 + 20);

    FuBytes *cloned = FuBytes_clone(bytes);
    assert(FuBytes_capacity(cloned) == FuBytes_len(bytes));
    assert(FuBytes_eq(&cloned, &bytes));

    FuBytes_drop(bytes);
    FuBytes_drop(cloned);
}

void test_bytes_init(void) {
    FuBytes bytes;
    FuBytes *bytes_ptr;

    FuBytes_init(&bytes);

    assert(bytes.len == 0);
    assert(bytes.cap == 0);
    assert(bytes.chars == NULL);

    FuBytes_push(&bytes, 'a');
    assert(bytes.len == 1);
    assert(bytes.chars[0] == 'a');

    FuBytes_push(&bytes, 'b');
    assert(bytes.len == 2);
    assert(bytes.chars[1] == 'b');

    FuBytes_reserve(&bytes, 2);
    assert(bytes.len == 2);
    assert(bytes.chars != NULL);

    FuBytes_reserve(&bytes, 20);
    assert(bytes.cap == 2 + 20);

    FuBytes *cloned = FuBytes_clone(&bytes);
    assert(FuBytes_capacity(cloned) == FuBytes_len(&bytes));
    bytes_ptr = &bytes;
    assert(FuBytes_eq(&cloned, &bytes_ptr));

    FuBytes_deinit(&bytes);
    FuBytes_drop(cloned);
}

void test_from_slice(void) {
    FuBytes *bytes = FuBytes_new();
    FuBytes_push(bytes, 'a');
    FuBytes_push(bytes, 'b');
    FuBytes_push(bytes, 'c');
    FuBytes_push(bytes, 'd');
    FuBytes *sub = FuBytes_from_slice(bytes, 1, 2);
    assert(FuBytes_len(sub) == 2);
    assert(FuBytes_get_byte(sub, 0) == 'b');
    assert(FuBytes_get_byte(sub, 1) == 'c');
    FuBytes_drop(bytes);
    FuBytes_drop(sub);
}

int main(void) {
    test_bytes_new();
    test_bytes_init();
    test_from_slice();
    return 0;
}
