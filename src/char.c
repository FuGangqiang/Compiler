#include <assert.h>

#include "char.h"

/* define char data */
#include "char.def"

/*
 * 返回值 =0: 不在区间内
 * 返回值 =1: 在区间内
 */
static fu_bool_t FuChar_binary_search_table(FuChar *table, int len, FuChar fc) {
    assert(len % 2 == 0);

    if (fc < table[0] || fc > table[len - 1]) {
        return FU_FALSE;
    }

    int lo = 0;
    int hi = len - 1;
    int mid;
    while (lo <= hi) {
        mid = lo + (hi - lo) / 2;
        if (fc < table[mid]) {
            hi = mid - 1;
        } else if (fc > table[mid]) {
            lo = mid + 1;
        } else {
            break;
        }
    }

    if (lo <= hi) {
        return FU_TRUE;
    }
    if (hi % 2 == 0) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_ascii(FuChar fc) {
    if (fc >= 0 && fc < 128) {
        return FU_TRUE;
    } else {
        return FU_FALSE;
    }
}

fu_bool_t FuChar_is_id_start(FuChar fc) {
    if ('a' <= fc && fc <= 'z') {
        return FU_TRUE;
    }
    if ('A' <= fc && fc <= 'Z') {
        return FU_TRUE;
    }
    if (fc == '_') {
        return FU_TRUE;
    }
    int len = sizeof(XID_START_TABLE) / sizeof(FuChar);
    if (fc > 0x7f && FuChar_binary_search_table(XID_START_TABLE, len, fc)) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_id_continue(FuChar fc) {
    if ('a' <= fc && fc <= 'z') {
        return FU_TRUE;
    }
    if ('A' <= fc && fc <= 'Z') {
        return FU_TRUE;
    }
    if ('0' <= fc && fc <= '9') {
        return FU_TRUE;
    }
    if (fc == '_') {
        return FU_TRUE;
    }
    int len = sizeof(XID_CONTINUE_TABLE) / sizeof(FuChar);
    if (fc > 0x7f && FuChar_binary_search_table(XID_CONTINUE_TABLE, len, fc)) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_whitespace(FuChar fc) {
    switch (fc) {
    /* horizontal whitespace */
    case 0x0009: /* \t */
    case 0x0020: /* space */
    case 0x00a0: /* no-break space */
    case 0x1680: /* ogham space mark */
    case 0x2000: /* en quad */
    case 0x2001: /* em quad */
    case 0x2002: /* en space */
    case 0x2003: /* em space */
    case 0x2004: /* three-per-em space */
    case 0x2005: /* four-per-em space */
    case 0x2006: /* six-per-em space */
    case 0x2007: /* figure space */
    case 0x2008: /* punctuation space */
    case 0x2009: /* thin space */
    case 0x200a: /* hair space */
    case 0x200e: /* left-to-right mark */
    case 0x200f: /* left-to-right mark */
    case 0x202f: /* narrow no-break space */
    case 0x205f: /* medium mathematical space */
    case 0x3000: /* ideographic space */

    /* vertical whitespace */
    case 0x000b: /* vertical tab */
    case 0x000c: /* form feed */
    case 0x000d: /* \r */
    case 0x0085: /* next line from latin1 */
    case 0x2028: /* line separator */
    case 0x2029: /* paragraph separator */
        return FU_TRUE;
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_newline(FuChar fc) {
    switch (fc) {
    case 0x000A: /* \n */
        return FU_TRUE;
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_binary_digit(FuChar fc) {
    if ('0' == fc || fc == '1') {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_octal_digit(FuChar fc) {
    if ('0' <= fc && fc <= '7') {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_decimal_digit(FuChar fc) {
    if ('0' <= fc && fc <= '9') {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_hexadecimal_digit(FuChar fc) {
    if ('0' <= fc && fc <= '9') {
        return FU_TRUE;
    } else if ('a' <= fc && fc <= 'f') {
        return FU_TRUE;
    } else if ('A' <= fc && fc <= 'F') {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuChar_is_lowercase_hexadecimal_digit(FuChar fc) {
    if ('0' <= fc && fc <= '9') {
        return FU_TRUE;
    } else if ('a' <= fc && fc <= 'f') {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_size_t FuChar_digit_to_uint(FuChar fc, fu_size_t base) {
    switch (base) {
    case 2:
        assert(FuChar_is_binary_digit(fc));
        return fc - '0';
    case 8:
        assert(FuChar_is_octal_digit(fc));
        return fc - '0';
    case 10:
        assert(FuChar_is_decimal_digit(fc));
        return fc - '0';
    case 16:
        assert(FuChar_is_lowercase_hexadecimal_digit(fc));
        if ('0' <= fc && fc <= '9') {
            return fc - '0';
        } else if ('a' <= fc && fc <= 'f') {
            return fc - 'a' + 10;
        } else if ('A' <= fc && fc <= 'F') {
            return fc - 'A' + 10;
        }
    default:
        /* can not be here */
        assert(0);
        return 0;
    }
}

/*
 * 返回值: 经转换而处理的 bytes 数量
 * 返回值 >0: 正确转换
 * 返回值 <=0: 在第 abs(返回值) 处出现转换错误
 */
int FuChar_from_utf8(FuChar *fc, const char *bytes, fu_size_t len) {
    unsigned int c0, c1, c2, c3;
    unsigned long ul;

    if (len <= 0) {
        *fc = UTF8_ERROR;
        return 0;
    }

    c0 = *(unsigned char *)bytes;
    if (c0 < UTF_HEADX) {
        *fc = (FuChar)c0;
        return 1;
    }

    if (c0 < UTF_HEAD2) {
        *fc = UTF8_ERROR;
        return -1;
    }

    if (len <= 1) {
        *fc = UTF8_ERROR;
        return -1;
    }

    c1 = *(unsigned char *)(bytes + 1) ^ UTF_HEADX;
    if (c1 & UTF8_TESTX) {
        *fc = UTF8_ERROR;
        return -1;
    }
    if (c0 < UTF_HEAD3) {
        ul = ((c0 << UTF8_BITX) | c1) & UTF8_MASK2;
        if (ul <= UTF8_MASK1) {
            *fc = UTF8_ERROR;
            return -2;
        }
        *fc = (FuChar)ul;
        return 2;
    }

    if (len <= 2) {
        *fc = UTF8_ERROR;
        return -2;
    }

    c2 = *(unsigned char *)(bytes + 2) ^ UTF_HEADX;
    if (c2 & UTF8_TESTX) {
        *fc = UTF8_ERROR;
        return -2;
    }
    if (c0 < UTF_HEAD4) {
        ul = ((((c0 << UTF8_BITX) | c1) << UTF8_BITX) | c2) & UTF8_MASK3;
        if (ul <= UTF8_MASK2) {
            *fc = UTF8_ERROR;
            return -3;
        }
        if (UTF8_SURROGATE_MIN <= ul && ul <= UTF8_SURROGATE_MAX) {
            *fc = UTF8_ERROR;
            return -3;
        }
        *fc = (FuChar)ul;
        return 3;
    }

    if (len <= 3) {
        *fc = UTF8_ERROR;
        return -3;
    }

    c3 = *(unsigned char *)(bytes + 3) ^ UTF_HEADX;
    if (c3 & UTF8_TESTX) {
        *fc = UTF8_ERROR;
        return -3;
    }
    if (c0 < UTF_HEAD5) {
        ul = ((((((c0 << UTF8_BITX) | c1) << UTF8_BITX) | c2) << UTF8_BITX) | c3) & UTF8_MASK4;
        if (ul <= UTF8_MASK3 || ul > UTF8_MAX) {
            *fc = UTF8_ERROR;
            return -4;
        }
        *fc = (FuChar)ul;
        return 4;
    }

    *fc = UTF8_ERROR;
    return -4;
}

/*
 * 返回值: 转换后字节数
 * bytes 参数：应确保有 4 字节可写
 */
fu_size_t FuChar_to_utf8(char *bytes, fu_size_t len, FuChar fc) {
    if (fc <= UTF8_MASK1) {
        if (len < 1) {
            goto bad_len;
        }
        bytes[0] = (char)fc;
        return 1;
    }

    if (fc <= UTF8_MASK2) {
        if (len < 2) {
            goto bad_len;
        }
        bytes[0] = (char)(UTF_HEAD2 | (fc >> 1 * UTF8_BITX));
        bytes[1] = (char)(UTF_HEADX | (fc & UTF8_MASKX));
        return 2;
    }

    if (fc > UTF8_MAX) {
        fc = UTF8_ERROR;
    }
    if (UTF8_SURROGATE_MIN <= fc && fc <= UTF8_SURROGATE_MAX) {
        fc = UTF8_ERROR;
    }

    if (fc <= UTF8_MASK3) {
        if (len < 3) {
            goto bad_len;
        }
        bytes[0] = (char)(UTF_HEAD3 | (fc >> 2 * UTF8_BITX));
        bytes[1] = (char)(UTF_HEADX | ((fc >> 1 * UTF8_BITX) & UTF8_MASKX));
        bytes[2] = (char)(UTF_HEADX | (fc & UTF8_MASKX));
        return 3;
    }

    if (len < 4) {
        goto bad_len;
    }
    bytes[0] = (char)(UTF_HEAD4 | (fc >> 3 * UTF8_BITX));
    bytes[1] = (char)(UTF_HEADX | ((fc >> 2 * UTF8_BITX) & UTF8_MASKX));
    bytes[2] = (char)(UTF_HEADX | ((fc >> 1 * UTF8_BITX) & UTF8_MASKX));
    bytes[3] = (char)(UTF_HEADX | (fc & UTF8_MASKX));
    return 4;

bad_len:
    return 0;
}

/*
 * 返回值: fc 转换后的字节
 */
fu_size_t FuChar_len_utf8(FuChar fc) {
    char bytes[8];
    return FuChar_to_utf8(bytes, 8, fc);
}
