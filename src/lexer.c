#include "alloc.h"
#include "log.h"
#include "parse.h"

FuLexer *FuLexer_new(FuCtx *ctx) {
    FuLexer *l = (FuLexer *)FuMem_malloc(sizeof(FuLexer));
    l->ctx = ctx;
    l->fpath = 0;
    l->chars = NULL;
    l->cur_line = 1;
    l->cur_column = 1;
    l->cursor = 0;
    l->tok_buf = FuVec_new(sizeof(FuToken));
    return l;
}

void FuLexer_drop(FuLexer *l) {
    if (!l) {
        return;
    }
    FuVec_drop(l->tok_buf);
    FuMem_free(l);
}

void FuLexer_for_file(FuLexer *l, char *fpath, fu_size_t len) {
    FuStr *fp = FuStr_new();
    FuStr_reserve(fp, len);
    FuStr_push_utf8(fp, fpath, len);
    fu_sym_t sym = FuCtx_intern_symbol(l->ctx, fp);
    l->fpath = sym;

    FuStr *fcontent = FuStr_new();
    FuStr_read_file(fcontent, fpath, len);
    FuCtx_intern_file(l->ctx, sym, fcontent);
    l->chars = fcontent;
}

/* n = ..., -2, -1, 0, 1, 2, ... */
static FuChar FuLexer_nth_char(FuLexer *l, long n) {
    fu_size_t len = FuStr_len(l->chars);
    long i = (long)l->cursor + n;
    if (i < 0 || i >= len) {
        return FuChar_EOF;
    }
    return FuStr_get_char(l->chars, i);
}

static FuChar FuLexer_first(FuLexer *l) {
    return FuLexer_nth_char(l, 0);
}

static FuChar FuLexer_second(FuLexer *l) {
    return FuLexer_nth_char(l, 1);
}

fu_bool_t FuLexer_is_eof(FuLexer *l) {
    return FuLexer_nth_char(l, 0) == FuChar_EOF;
}

/* moves to next char */
static FuChar FuLexer_bump(FuLexer *l) {
    FuChar fc = FuLexer_nth_char(l, 0);
    if (fc != FuChar_EOF) {
        l->cursor++;
    }
    if (fc == '\n') {
        l->cur_line++;
        l->cur_column = 1;
    } else {
        l->cur_column++;
    }
    return fc;
}

/* moves to prev char */
static void FuLexer_unbump(FuLexer *l) {
    if (l->cursor > 0) {
        l->cursor--;
        l->cur_column--;
    }
}

static fu_bool_t FuLexer_repeat_char_until(FuLexer *l, FuChar repeat, FuChar to) {
    fu_size_t n = 0;
    FuChar fc;
    while ((fc = FuLexer_nth_char(l, n)) == repeat) {
        n++;
    }
    if (fc == to) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static fu_bool_t FuLexer_eat_decimal_digits(FuLexer *l) {
    fu_bool_t has_digits = FU_FALSE;

    FuChar fc;
    while (1) {
        fc = FuLexer_first(l);
        if (fc == '_') {
            FuLexer_bump(l);
        } else if (FuChar_is_decimal_digit(fc)) {
            has_digits = FU_TRUE;
            FuLexer_bump(l);
        } else {
            break;
        }
    }

    return has_digits;
}

static fu_bool_t FuLexer_eat_float_exponent(FuLexer *l) {
    FuChar fc;
    fc = FuLexer_first(l);
    if (fc == '-' || fc == '+') {
        FuLexer_bump(l);
    }
    return FuLexer_eat_decimal_digits(l);
}

static void FuLexer_eat_identifier(FuLexer *l) {
    if (!FuChar_is_id_start(FuLexer_first(l))) {
        return;
    }
    FuLexer_bump(l);
    while (FuChar_is_id_continue(FuLexer_first(l))) {
        FuLexer_bump(l);
    }
}

static void FuLexer_eat_literal_suffix(FuLexer *l) {
    FuLexer_eat_identifier(l);
}

static fu_bool_t FuLexer_eat_hexadecimal_digits(FuLexer *l) {
    int has_digits = FU_FALSE;

    FuChar fc;
    while (1) {
        fc = FuLexer_first(l);
        if (fc == '_') {
            FuLexer_bump(l);
        } else if (FuChar_is_hexadecimal_digit(fc)) {
            has_digits = FU_TRUE;
            FuLexer_bump(l);
        } else {
            break;
        }
    }

    return has_digits;
}

static fu_bool_t FuLexer_eat_single_quoted_string(FuLexer *l) {
    if (FuLexer_first(l) != '\\' && FuLexer_second(l) == '\'') {
        FuLexer_bump(l);
        FuLexer_bump(l);
        return FU_TRUE;
    }

    /* literal has more than one symbol */
    FuChar fc;
    while (1) {
        fc = FuLexer_first(l);
        /* quotes are terminated, finish parsing */
        if (fc == '\'') {
            FuLexer_bump(l);
            return FU_TRUE;
        }
        /* probably beginning of the comment, which we don't want to include to the error report */
        if (fc == '/') {
            break;
        }
        /* newline without following '\'' means unclosed quote, stop parsing */
        if (fc == '\n' && FuLexer_second(l) != '\'') {
            break;
        }
        /* end of file, stop parsing */
        if (fc == FuChar_EOF) {
            break;
        }
        /* escaped slash is considered one character, so bump twice */
        if (fc == '\\') {
            FuLexer_bump(l);
            FuLexer_bump(l);
        } else {
            FuLexer_bump(l);
        }
    }
    return FU_FALSE;
}

static fu_size_t _get_string_prefix_ignore(FuLexer *l, fu_size_t suffix) {
    fu_size_t n = 0;
    long i = -(long)(suffix + 1);
    while (FuLexer_nth_char(l, i) == ' ') {
        i--;
        n++;
    }
    if (FuLexer_nth_char(l, i) == '\n') {
        return n;
    }
    return 0;
}

static fu_bool_t FuLexer_eat_double_quoted_string(FuLexer *l, fu_size_t *prefix_ignore) {
    FuChar fc;

    *prefix_ignore = 0;
    do {
        fc = FuLexer_bump(l);
        if (fc == '"') {
            *prefix_ignore = _get_string_prefix_ignore(l, 1);
            return FU_TRUE;
        }
        if (fc == '\\') {
            if (FuLexer_first(l) == '\\' || FuLexer_first(l) == '"') {
                FuLexer_bump(l);
            }
        }
    } while (fc != FuChar_EOF);

    return FU_FALSE;
}

static fu_bool_t FuLexer_eat_raw_double_quoted_string(FuLexer *l, fu_size_t *n_hashes, fu_bool_t *started,
                                                      fu_size_t *prefix_ignore) {
    *prefix_ignore = 0;
    *n_hashes = 0;
    *started = FU_FALSE;
    while (FuLexer_first(l) == '#') {
        *n_hashes += 1;
        FuLexer_bump(l);
    }

    if (FuLexer_bump(l) == '"') {
        *started = FU_TRUE;
    } else {
        return FU_FALSE;
    }

    /* skip the string contents and on each '#' character met, check if this is a string termination */
    while (1) {
        while (FuLexer_first(l) != '"') {
            FuLexer_bump(l);
            if (FuLexer_is_eof(l)) {
                return FU_FALSE;
            }
        }

        /* eat closing double quote */
        FuLexer_bump(l);

        /* check amount of closing '#' symbols */
        fu_size_t hashes_left = *n_hashes;
        while (FuLexer_first(l) == '#' && hashes_left != 0) {
            FuLexer_bump(l);
            hashes_left -= 1;
        }

        if (hashes_left == 0) {
            *prefix_ignore = _get_string_prefix_ignore(l, *n_hashes + 1);
            return FU_TRUE;
        }
    }
}

static FuSpan *FuLexer_token_span(FuLexer *l, fu_size_t start) {
    FuSpan *sp = FuSpan_new(l->ctx, l->fpath, start, l->cursor - start, l->tok_line, l->tok_column);
    return sp;
}

static fu_sym_t FuLexer_token_sym(FuLexer *l, fu_size_t start, fu_size_t len) {
    FuStr *str = FuStr_from_slice(l->chars, start, len);
    fu_sym_t sym = FuCtx_intern_symbol(l->ctx, str);
    return sym;
}

/*
 * start 和 len 为去除了首尾的引号和井号后的内容
 */
static fu_sym_t FuLexer_token_str_sym(FuLexer *l, FuSpan *sp, fu_size_t start, fu_size_t len, fu_size_t prefix_ignore) {
    fu_size_t offset = start - sp->start;
    FuStr *str = FuStr_with_capacity(len);

    /* 忽略第一个换行符 */
    if (FuStr_get_char(l->chars, start) == '\n') {
        start++;
        len--;
        offset++;
    }

    fu_size_t i;
    /* 忽略最后 prefix_ignore 样本 */
    for (i = 0; i < prefix_ignore; i++) {
        if (FuStr_get_char(l->chars, start + len - 1) == ' ') {
            len -= 1;
        }
    }
    if (FuStr_get_char(l->chars, start + len - 1) == '\n') {
        len -= 1;
    }

    fu_size_t j;
    FuChar fc;
    i = 0;
    while (i < len) {
        /* 忽略 prefix ignore 空格 */
        for (j = 0; j < prefix_ignore; j++) {
            fc = FuStr_get_char(l->chars, start + i);
            if (fc == '\n') {
                break;
            }
            if (fc != ' ') {
                FuSpan *err_span = FuSpan_offset(sp, offset);
                FATAL(err_span, "only `space` allow in prefix strips");
            }
            i++;
            offset++;
        }

        if (i >= len) {
            break;
        }
        do {
            fc = FuStr_get_char(l->chars, start + i);
            FuStr_push(str, fc);
            i++;
            offset++;
        } while (fc != '\n' && i < len);
    }

    fu_sym_t sym = FuCtx_intern_symbol(l->ctx, str);
    return sym;
}

static FuToken FuLexer_comment(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    FuLexer_bump(l);

    FuChar fc;
    do {
        fc = FuLexer_bump(l);
        if (fc == '\n') {
            break;
        }
    } while (fc != FuChar_EOF);
    FuSpan *sp = FuLexer_token_span(l, start);
    FuChar third_fc = FuStr_get_char(l->chars, start + 2);
    if (third_fc == '!' || third_fc == '/') {
        fu_sym_t sym = FuLexer_token_sym(l, start + 2, sp->len - 3);
        return FuToken_new_doc_comment(sp, sym);
    } else {
        return FuToken_new(TOK_COMMENT, sp);
    }
}

static FuToken FuLexer_whitespace(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    while (FuChar_is_whitespace(FuLexer_first(l))) {
        FuLexer_bump(l);
    }
    FuSpan *sp = FuLexer_token_span(l, start);
    return FuToken_new(TOK_WHITESPACE, sp);
}

static FuToken FuLexer_newline(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    while (FuChar_is_newline(FuLexer_first(l))) {
        FuLexer_bump(l);
    }
    FuSpan *sp = FuLexer_token_span(l, start);
    return FuToken_new(TOK_NEWLINE, sp);
}

static FuToken FuLexer_identifier(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    while (FuChar_is_id_continue(FuLexer_first(l))) {
        FuLexer_bump(l);
    }
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_sym(l, start, sp->len);
    return FuToken_new_ident(sp, sym);
}

static FuToken FuLexer_raw_ident(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    FuLexer_bump(l);
    FuLexer_eat_identifier(l);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_sym(l, start + 2, sp->len - 2);
    return FuToken_new_raw_ident(sp, sym);
}

static FuToken FuLexer_number(FuLexer *l, FuChar first_digit) {
    fu_size_t start = l->cursor - 1;

    FuChar fc;
    int base = 10;
    if (first_digit == '0') {
        fu_bool_t has_digits;

        fc = FuLexer_first(l);
        if (fc == 'b') {
            base = 2;
            FuLexer_bump(l);
            has_digits = FuLexer_eat_decimal_digits(l);
        } else if (fc == 'o') {
            base = 8;
            FuLexer_bump(l);
            has_digits = FuLexer_eat_decimal_digits(l);
        } else if (fc == 'x') {
            base = 16;
            FuLexer_bump(l);
            has_digits = FuLexer_eat_hexadecimal_digits(l);
        } else if (('0' <= fc && fc <= '9') || fc == '_' || fc == '.' || fc == 'e' || fc == 'E') {
            /* Not a base prefix */
            FuLexer_eat_decimal_digits(l);
            has_digits = FU_TRUE;
        } else {
            /* just 0 */
            fu_size_t suffix_start = l->cursor - start;
            FuLexer_eat_literal_suffix(l);
            fu_size_t len = l->cursor - start;
            FuSpan *sp = FuLexer_token_span(l, start);
            fu_sym_t sym = FuLexer_token_sym(l, start, len);
            return FuToken_new_lit_int(sp, sym, base, FU_FALSE, suffix_start);
        }

        /* base prefix was provided, but there were no digits after it, e.g. "0x" */
        if (!has_digits) {
            fu_size_t suffix_start = l->cursor - start;
            FuLexer_eat_literal_suffix(l);
            fu_size_t len = l->cursor - start;
            FuSpan *sp = FuLexer_token_span(l, start);
            fu_sym_t sym = FuLexer_token_sym(l, start, len);
            return FuToken_new_lit_int(sp, sym, base, FU_TRUE, suffix_start);
        }
    } else {
        /* no base prefix */
        FuLexer_eat_decimal_digits(l);
    }

    fc = FuLexer_first(l);
    if (fc == 'e' || fc == 'E') {
        FuLexer_bump(l);
        fu_bool_t empty_exponent = !FuLexer_eat_float_exponent(l);
        fu_size_t suffix_start = l->cursor - start;
        FuLexer_eat_literal_suffix(l);
        fu_size_t len = l->cursor - start;
        FuSpan *sp = FuLexer_token_span(l, start);
        fu_sym_t sym = FuLexer_token_sym(l, start, len);
        return FuToken_new_lit_float(sp, sym, base, empty_exponent, suffix_start);
    }

    /* Don't be greedy if  an integer literal followed by field/method access or a range pattern
     * (`0..2` and `12.foo()`) */
    FuChar second = FuLexer_second(l);
    if (fc == '.' && second != '.' && !FuChar_is_id_start(second)) {
        FuLexer_bump(l);
        fu_bool_t empty_exponent = FU_FALSE;
        if (FuChar_is_decimal_digit(FuLexer_first(l))) {
            FuLexer_eat_decimal_digits(l);
            fc = FuLexer_first(l);
            if (fc == 'e' || fc == 'E') {
                FuLexer_bump(l);
                empty_exponent = !FuLexer_eat_float_exponent(l);
            }
        }
        fu_size_t suffix_start = l->cursor - start;
        FuLexer_eat_literal_suffix(l);
        fu_size_t len = l->cursor - start;
        FuSpan *sp = FuLexer_token_span(l, start);
        fu_sym_t sym = FuLexer_token_sym(l, start, len);
        return FuToken_new_lit_float(sp, sym, base, empty_exponent, suffix_start);
    }

    fu_size_t suffix_start = l->cursor - start;
    FuLexer_eat_literal_suffix(l);
    fu_size_t len = l->cursor - start;
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_sym(l, start, len);
    return FuToken_new_lit_int(sp, sym, base, FU_FALSE, suffix_start);
}

static FuToken FuLexer_label_or_char(FuLexer *l) {
    fu_size_t start = l->cursor - 1;

    fu_bool_t can_be_a_label;
    if (FuLexer_second(l) == '\'') {
        can_be_a_label = FU_FALSE;
    } else {
        FuChar fc = FuLexer_first(l);
        if (FuChar_is_id_start(fc) || FuChar_is_decimal_digit(fc)) {
            can_be_a_label = FU_TRUE;
        } else {
            can_be_a_label = FU_FALSE;
        }
    }

    if (!can_be_a_label) {
        fu_bool_t terminated = FuLexer_eat_single_quoted_string(l);
        FuSpan *sp = FuLexer_token_span(l, start);
        fu_sym_t sym = FuLexer_token_sym(l, start + 1, sp->len - 2);
        return FuToken_new_lit_char(sp, sym, terminated);
    }

    /* either label or a character literal with length greater than 1 */
    FuLexer_bump(l);
    while (FuChar_is_id_continue(FuLexer_first(l))) {
        FuLexer_bump(l);
    }
    if (FuLexer_first(l) == '\'') {
        FuLexer_bump(l);
        FuSpan *sp = FuLexer_token_span(l, start);
        fu_sym_t sym = FuLexer_token_sym(l, start + 1, sp->len - 2);
        return FuToken_new_lit_char(sp, sym, FU_TRUE);
    } else {
        FuSpan *sp = FuLexer_token_span(l, start);
        fu_sym_t sym = FuLexer_token_sym(l, start, sp->len);
        return FuToken_new_lable(sp, sym);
    }
}

/* b'x' */
static FuToken FuLexer_single_quoted_byte(FuLexer *l) {
    size_t start = l->cursor - 1;
    FuLexer_bump(l);
    fu_bool_t terminated = FuLexer_eat_single_quoted_string(l);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + 2, sp->len - 3, 0);
    return FuToken_new_lit_byte(sp, sym, terminated);
}

/* "xxx" */
static FuToken FuLexer_double_quoted_string(FuLexer *l) {
    fu_size_t start = l->cursor - 1;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_double_quoted_string(l, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + 1, sp->len - 2, prefix_ignore);
    return FuToken_new_lit_str(TOK_STR, sp, sym, 0, FU_TRUE, prefix_ignore, terminated);
}

/* #"xxx"# */
static FuToken FuLexer_hash_double_quoted_string(FuLexer *l) {
    fu_size_t start = l->cursor;
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 1, sp->len - n_hashes * 2 - 2, prefix_ignore);
    return FuToken_new_lit_str(TOK_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

/* r"xxx" */
static FuToken FuLexer_raw_double_quoted_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 2, sp->len - n_hashes * 2 - 3, prefix_ignore);
    return FuToken_new_lit_str(TOK_RAW_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

/* b"xxx" */
static FuToken FuLexer_double_quoted_byte_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    FuLexer_bump(l);
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_double_quoted_string(l, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + 2, sp->len - 3, prefix_ignore);
    return FuToken_new_lit_str(TOK_BYTE_STR, sp, sym, 0, FU_TRUE, prefix_ignore, terminated);
}

/* b#"xxx"# */
static FuToken FuLexer_hash_double_quoted_byte_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 2, sp->len - n_hashes * 2 - 3, prefix_ignore);
    return FuToken_new_lit_str(TOK_BYTE_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

/* br"xxx" */
static FuToken FuLexer_raw_double_quoted_byte_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    FuLexer_bump(l);
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 3, sp->len - n_hashes * 2 - 4, prefix_ignore);
    return FuToken_new_lit_str(TOK_BYTE_RAW_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

/* f"xxx" */
static FuToken FuLexer_double_quoted_format_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    FuLexer_bump(l);
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_double_quoted_string(l, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + 2, sp->len - 3, prefix_ignore);
    return FuToken_new_lit_str(TOK_FORMAT_STR, sp, sym, 0, FU_TRUE, prefix_ignore, terminated);
}

/* f#"xxx"# */
static FuToken FuLexer_hash_double_quoted_format_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 2, sp->len - n_hashes * 2 - 3, prefix_ignore);
    return FuToken_new_lit_str(TOK_FORMAT_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

/* fr#"xxx"# */
static FuToken FuLexer_raw_double_quoted_format_string(FuLexer *l) {
    size_t start = l->cursor - 1;
    FuLexer_bump(l);
    fu_size_t n_hashes;
    fu_bool_t started;
    fu_size_t prefix_ignore;
    fu_bool_t terminated = FuLexer_eat_raw_double_quoted_string(l, &n_hashes, &started, &prefix_ignore);
    FuSpan *sp = FuLexer_token_span(l, start);
    fu_sym_t sym = FuLexer_token_str_sym(l, sp, start + n_hashes + 3, sp->len - n_hashes * 2 - 4, prefix_ignore);
    return FuToken_new_lit_str(TOK_FORMAT_RAW_STR, sp, sym, n_hashes, started, prefix_ignore, terminated);
}

void FuLexer_unget_token(FuLexer *l, FuToken tok) {
    if (tok.kd == TOK_EOF) {
        return;
    }
    FuVec_push(l->tok_buf, &tok);
}

FuToken FuLexer_get_token(FuLexer *l) {
    if (!FuVec_is_empty(l->tok_buf)) {
        FuToken tok;
        FuVec_pop(l->tok_buf, &tok);
        return tok;
    }
    l->tok_line = l->cur_line;
    l->tok_column = l->cur_column;

    FuChar first_char = FuLexer_bump(l);

    /* whitespace */
    if (FuChar_is_whitespace(first_char)) {
        return FuLexer_whitespace(l);
    }

    /* slash, comment */
    if (first_char == '/') {
        if (FuLexer_first(l) == '/') {
            return FuLexer_comment(l);
        }
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_SLASH, sp);
    }

    /* raw string literal, raw identifier or identifier */
    if (first_char == 'r') {
        FuChar fst = FuLexer_first(l);
        FuChar snd = FuLexer_second(l);
        if (fst == '#' && FuChar_is_id_start(snd)) {
            return FuLexer_raw_ident(l);
        }
        if (fst == '#' || fst == '"') {
            return FuLexer_raw_double_quoted_string(l);
        }
        return FuLexer_identifier(l);
    }

    /* format string literal, raw format string literal or identifier */
    if (first_char == 'f') {
        FuChar fst = FuLexer_first(l);
        if (fst == '"') {
            return FuLexer_double_quoted_format_string(l);
        }
        if (FuLexer_repeat_char_until(l, '#', '"')) {
            return FuLexer_hash_double_quoted_format_string(l);
        }
        if (fst == 'r') {
            FuChar snd = FuLexer_second(l);
            if (snd == '"' || snd == '#') {
                return FuLexer_raw_double_quoted_format_string(l);
            }
        }
        return FuLexer_identifier(l);
    }

    /* byte literal, byte string literal, raw byte string literal or identifier */
    if (first_char == 'b') {
        FuChar fst = FuLexer_first(l);
        if (fst == '\'') {
            return FuLexer_single_quoted_byte(l);
        }
        if (fst == '"') {
            return FuLexer_double_quoted_byte_string(l);
        }
        if (FuLexer_repeat_char_until(l, '#', '"')) {
            return FuLexer_hash_double_quoted_byte_string(l);
        }
        if (fst == 'r') {
            FuChar snd = FuLexer_second(l);
            if (snd == '"' || snd == '#') {
                return FuLexer_raw_double_quoted_byte_string(l);
            }
        }
        return FuLexer_identifier(l);
    }

    /* identifier */
    if (FuChar_is_id_start(first_char)) {
        return FuLexer_identifier(l);
    }

    /* number literal */
    if (FuChar_is_decimal_digit(first_char)) {
        return FuLexer_number(l, first_char);
    }

    if (FuChar_is_newline(first_char)) {
        return FuLexer_newline(l);
    }

    if (first_char == ';') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_SEMI, sp);
    }
    if (first_char == ',') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_COMMA, sp);
    }
    if (first_char == '.') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_DOT, sp);
    }
    if (first_char == '(') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_OPEN_PAREN, sp);
    }
    if (first_char == ')') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_CLOSE_PAREN, sp);
    }
    if (first_char == '{') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_OPEN_BRACE, sp);
    }
    if (first_char == '}') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_CLOSE_BRACE, sp);
    }
    if (first_char == '[') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_OPEN_BRACKET, sp);
    }
    if (first_char == ']') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_CLOSE_BRACKET, sp);
    }

    if (first_char == '?') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_QUESTION, sp);
    }
    if (first_char == ':') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_COLON, sp);
    }
    if (first_char == '+') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_PLUS, sp);
    }
    if (first_char == '-') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_MINUS, sp);
    }
    if (first_char == '*') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_STAR, sp);
    }
    if (first_char == '%') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_PERCENT, sp);
    }
    if (first_char == '=') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_EQ, sp);
    }
    if (first_char == '!') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_NOT, sp);
    }
    if (first_char == '<') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_LT, sp);
    }
    if (first_char == '>') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_GT, sp);
    }
    if (first_char == '&') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_AND, sp);
    }
    if (first_char == '|') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_OR, sp);
    }
    if (first_char == '^') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_CARET, sp);
    }
    if (first_char == '@') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_AT, sp);
    }
    if (first_char == '$') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_DOLLAR, sp);
    }
    if (first_char == '~') {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_TILDE, sp);
    }

    /* string literal or pound(#) */
    if (first_char == '"') {
        return FuLexer_double_quoted_string(l);
    } else if (first_char == '#') {
        if (FuLexer_repeat_char_until(l, '#', '"')) {
            FuLexer_unbump(l);
            return FuLexer_hash_double_quoted_string(l);
        }
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_POUND, sp);
    }

    /* label or character literal */
    if (first_char == '\'') {
        return FuLexer_label_or_char(l);
    }

    /* EOF */
    if (first_char == FuChar_EOF) {
        FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
        return FuToken_new(TOK_EOF, sp);
    }

    /* unknown */
    FuSpan *sp = FuLexer_token_span(l, l->cursor - 1);
    return FuToken_new(TOK_UNKNOWN, sp);
}

FuStr *FuLexer_dump(FuLexer *l) {
    FuStr *dump = FuStr_new();
    FuToken token = FuLexer_get_token(l);
    while (!FuToken_is_eof(token)) {
        FuStr *token_str = FuToken_display(token);
        FuStr_append(dump, FuSpan_display(token.sp));
        FuStr_push_utf8_cstr(dump, ":");
        FuStr_append(dump, token_str);
        FuStr_push_utf8_cstr(dump, "\n");
        token = FuLexer_get_token(l);
    }
    return dump;
}
