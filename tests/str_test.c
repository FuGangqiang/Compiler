#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "str.h"

void test_str_new(void) {
    FuStr *str = FuStr_new();

    assert(str->len == 0);
    assert(str->cap == 0);
    assert(str->chars == NULL);

    FuStr_push(str, 'a');
    assert(str->len == 1);
    assert(str->chars[0] == 'a');

    FuStr_push(str, 'b');
    assert(str->len == 2);
    assert(str->chars[1] == 'b');

    FuStr_reserve(str, 2);
    assert(str->len == 2);
    assert(str->chars != NULL);

    FuStr_reserve(str, 20);
    assert(str->cap == 2 + 20);

    FuStr *cloned = FuStr_clone(str);
    assert(FuStr_capacity(cloned) == FuStr_len(str));
    assert(FuStr_eq(&cloned, &str));

    FuStr_drop(str);
    FuStr_drop(cloned);
}

void test_str_init(void) {
    FuStr str;
    FuStr *str_ptr;

    FuStr_init(&str);

    assert(str.len == 0);
    assert(str.cap == 0);
    assert(str.chars == NULL);

    FuStr_push(&str, 'a');
    assert(str.len == 1);
    assert(str.chars[0] == 'a');

    FuStr_push(&str, 'b');
    assert(str.len == 2);
    assert(str.chars[1] == 'b');

    FuStr_reserve(&str, 2);
    assert(str.len == 2);
    assert(str.chars != NULL);

    FuStr_reserve(&str, 20);
    assert(str.cap == 2 + 20);

    FuStr *cloned = FuStr_clone(&str);
    assert(FuStr_capacity(cloned) == FuStr_len(&str));
    str_ptr = &str;
    assert(FuStr_eq(&cloned, &str_ptr));

    FuStr_deinit(&str);
    FuStr_drop(cloned);
}

void test_utf8(void) {
    FuStr *str = FuStr_new();
    /* lambda */
    char *s1 = "\xce\xbb";
    FuChar fc1 = 0x3bb;
    FuStr_push_utf8(str, s1, strlen(s1));
    assert(str->len == 1);
    assert(str->chars[0] == fc1);

    /* 中 */
    char *s2 = "\xe4\xb8\xad";
    FuChar fc2 = 0x4e2d;
    FuStr_push_utf8(str, s2, strlen(s2));
    assert(str->len == 2);
    assert(str->chars[1] == fc2);

    /* :smile: */
    char *s3 = "\xf0\x9f\x98\x81";
    FuChar fc3 = 0x1f601;
    FuStr_push_utf8(str, s3, strlen(s3));
    assert(str->len == 3);
    assert(str->chars[2] == fc3);

    FuStr_print(stdout, str);
    printf("\n");
    FuStr_drop(str);
}

void test_from_slice(void) {
    FuStr *str = FuStr_new();
    FuStr_push(str, 'a');
    FuStr_push(str, 'b');
    FuStr_push(str, 'c');
    FuStr_push(str, 'd');
    FuStr *sub = FuStr_from_slice(str, 1, 2);
    assert(FuStr_len(sub) == 2);
    assert(FuStr_get_char(sub, 0) == 'b');
    assert(FuStr_get_char(sub, 1) == 'c');
    FuStr_drop(str);
    FuStr_drop(sub);
}

int main(void) {
    test_str_new();
    test_str_init();
    test_utf8();
    test_from_slice();
    return 0;
}
