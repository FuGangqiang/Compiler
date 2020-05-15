#include <assert.h>
#include <stdio.h>

#include "char.h"

void _test_utf8_char(char *s, int len, FuChar fc) {
    int count;
    char bytes[4];
    FuChar result;

    count = FuChar_from_utf8(&result, s, len);
    assert(count == len);
    assert(fc == result);

    count = FuChar_to_utf8(bytes, 4, fc);
    assert(count == len);
    int i;
    for (i = 0; i < len; i++) {
        assert(s[i] == bytes[i]);
    }

    count = FuChar_len_utf8(fc);
    assert(count == len);
}

void test_utf8(void) {
    char *s1 = "a";
    FuChar fc1 = 'a';
    /* lambda */
    char *s2 = "\xce\xbb";
    FuChar fc2 = 0x3bb;
    /* 中 */
    char *s3 = "\xe4\xb8\xad";
    FuChar fc3 = 0x4e2d;
    /* :smile: */
    char *s4 = "\xf0\x9f\x98\x81";
    FuChar fc4 = 0x1f601;

    _test_utf8_char(s1, 1, fc1);
    _test_utf8_char(s2, 2, fc2);
    _test_utf8_char(s3, 3, fc3);
    _test_utf8_char(s4, 4, fc4);
}

void test_whitesapce(void) {
    assert(FuChar_is_whitespace(' '));
    assert(FuChar_is_whitespace('\t'));
    assert(FuChar_is_whitespace('\v'));
    assert(!FuChar_is_whitespace('a'));
    assert(FuChar_is_whitespace('\f'));
    assert(FuChar_is_whitespace('\r'));
    assert(!FuChar_is_whitespace('\n'));
}

void test_is_id_start(void) {
    assert(FuChar_is_id_start('A'));
    assert(FuChar_is_id_start('B'));
    assert(FuChar_is_id_start('Z'));
    assert(FuChar_is_id_start('a'));
    assert(FuChar_is_id_start('B'));
    assert(FuChar_is_id_start('z'));
    assert(FuChar_is_id_start('_'));

    assert(FuChar_is_id_start(0x1000d));
    assert(FuChar_is_id_start(0x10026));
    assert(!FuChar_is_id_start(0x00));
    assert(!FuChar_is_id_start(0x01));
    assert(!FuChar_is_id_start(0x02c2));
    assert(!FuChar_is_id_start(0xffff));

    assert(FuChar_is_id_start(0x2c6));
    assert(FuChar_is_id_start(0x2c7));
    assert(FuChar_is_id_start(0x2d1));
    assert(!FuChar_is_id_start(0x2d2));
    assert(FuChar_is_id_start(0x2e0));
    assert(FuChar_is_id_start(0x2e1));
    assert(FuChar_is_id_start(0x2e4));
    assert(!FuChar_is_id_start(0x2e5));

    assert(!FuChar_is_id_start('0'));
    assert(!FuChar_is_id_start('1'));
    assert(!FuChar_is_id_start('9'));
    assert(!FuChar_is_id_start(' '));
    assert(!FuChar_is_id_start('['));
    assert(!FuChar_is_id_start('<'));
    assert(!FuChar_is_id_start('}'));
    assert(!FuChar_is_id_start('('));
}

void test_is_id_continue(void) {
    assert(FuChar_is_id_continue('0'));
    assert(FuChar_is_id_continue('1'));
    assert(FuChar_is_id_continue('9'));
    assert(FuChar_is_id_continue('A'));
    assert(FuChar_is_id_continue('B'));
    assert(FuChar_is_id_continue('Z'));
    assert(FuChar_is_id_continue('a'));
    assert(FuChar_is_id_continue('b'));
    assert(FuChar_is_id_continue('z'));
    assert(FuChar_is_id_continue('_'));

    assert(FuChar_is_id_continue(0x1000d));
    assert(FuChar_is_id_continue(0x10026));
    assert(!FuChar_is_id_continue(0x00));
    assert(!FuChar_is_id_continue(0x01));
    assert(!FuChar_is_id_continue(0x02c2));
    assert(!FuChar_is_id_continue(0xffff));

    assert(FuChar_is_id_continue(0x2c6));
    assert(FuChar_is_id_continue(0x2c7));
    assert(FuChar_is_id_continue(0x2d1));
    assert(!FuChar_is_id_continue(0x2d2));
    assert(FuChar_is_id_continue(0x2e0));
    assert(FuChar_is_id_continue(0x2e1));
    assert(FuChar_is_id_continue(0x2e4));
    assert(!FuChar_is_id_continue(0x2e5));

    assert(!FuChar_is_id_continue(' '));
    assert(!FuChar_is_id_continue('['));
    assert(!FuChar_is_id_continue('<'));
    assert(!FuChar_is_id_continue('}'));
    assert(!FuChar_is_id_continue('('));
}

int main(void) {
    test_utf8();
    test_whitesapce();
    test_is_id_start();
    test_is_id_continue();
    return 0;
}
