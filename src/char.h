#ifndef FU_CHAR_H
#define FU_CHAR_H

#include "def.h"

typedef fu_uint32_t FuChar;

/*
 * 1bytes  7bit from:U+0000   to:U+007F     bytes: 0xxxxxxx
 * 2bytes 11bit from:U+0080   to:U+07FF     bytes: 110xxxxx  10xxxxxx
 * 3bytes 16bit from:U+0800   to:U+FFFF     bytes: 1110xxxx  10xxxxxx  10xxxxxx
 * 4bytes 21bit from:U+10000  to:U+10FFFF   bytes: 11110xxx  10xxxxxx  10xxxxxx  10xxxxxx
 */
enum {
    FuChar_EOF = 0,
    UTF8_MAX = 0X10FFFF,
    UTF8_ERROR = 0xFFFD,
    UTF8_SURROGATE_MIN = 0xD800,
    UTF8_SURROGATE_MAX = 0xDFFF,

    UTF8_BIT1 = 7,
    UTF8_BIT2 = 5,
    UTF8_BIT3 = 4,
    UTF8_BIT4 = 3,
    UTF8_BIT5 = 2,
    UTF8_BITX = 6,

    UTF_HEAD1 = ((1 << (UTF8_BIT1 + 1)) - 1) ^ 0xFF, /* 0000 0000 */
    UTF_HEADX = ((1 << (UTF8_BITX + 1)) - 1) ^ 0xFF, /* 1000 0000 */
    UTF_HEAD2 = ((1 << (UTF8_BIT2 + 1)) - 1) ^ 0xFF, /* 1100 0000 */
    UTF_HEAD3 = ((1 << (UTF8_BIT3 + 1)) - 1) ^ 0xFF, /* 1110 0000 */
    UTF_HEAD4 = ((1 << (UTF8_BIT4 + 1)) - 1) ^ 0xFF, /* 1111 0000 */
    UTF_HEAD5 = ((1 << (UTF8_BIT5 + 1)) - 1) ^ 0xFF, /* 1111 1000 */

    UTF8_MASK1 = (1 << (UTF8_BIT1 + 0 * UTF8_BITX)) - 1, /* 0000 0000 0111 1111 */
    UTF8_MASK2 = (1 << (UTF8_BIT2 + 1 * UTF8_BITX)) - 1, /* 0000 0111 1111 1111 */
    UTF8_MASK3 = (1 << (UTF8_BIT3 + 2 * UTF8_BITX)) - 1, /* 1111 1111 1111 1111 */
    UTF8_MASK4 = (1 << (UTF8_BIT4 + 3 * UTF8_BITX)) - 1, /* 0001 1111 1111 1111 1111 1111 */
    UTF8_MASKX = (1 << UTF8_BITX) - 1,                   /* 0011 1111 */
    UTF8_TESTX = UTF8_MASKX ^ 0xFF,                      /* 1100 0000 */
};

fu_bool_t FuChar_is_ascii(FuChar fc);
fu_bool_t FuChar_is_id_start(FuChar fc);
fu_bool_t FuChar_is_id_continue(FuChar fc);
fu_bool_t FuChar_is_whitespace(FuChar fc);
fu_bool_t FuChar_is_newline(FuChar fc);
fu_bool_t FuChar_is_binary_digit(FuChar fc);
fu_bool_t FuChar_is_octal_digit(FuChar fc);
fu_bool_t FuChar_is_decimal_digit(FuChar fc);
fu_bool_t FuChar_is_hexadecimal_digit(FuChar fc);
fu_bool_t FuChar_is_lowercase_hexadecimal_digit(FuChar fc);

fu_size_t FuChar_digit_to_uint(FuChar fc, fu_size_t base);

int FuChar_from_utf8(FuChar *fc, const char *bytes, fu_size_t len);
fu_size_t FuChar_to_utf8(char *bytes, fu_size_t len, FuChar fc);
fu_size_t FuChar_len_utf8(FuChar fc);

#endif /* FU_CHAR_H */
