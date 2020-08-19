#include <assert.h>
#include <stdarg.h>

#include "alloc.h"
#include "error.h"
#include "parse.h"

FuParser *FuParser_new(FuContext *ctx) {
    FuParser *p = FuMem_new(FuParser);
    p->ctx = ctx;
    p->tok_buf = FuVec_new(sizeof(FuToken));
    p->cursor = 0;
    p->unclosed_delims = FuVec_new(sizeof(FuToken));
    return p;
}

void FuParser_drop(FuParser *p) {
    if (!p) {
        return;
    }
    FuVec_drop(p->unclosed_delims);
    FuVec_drop(p->tok_buf);
    FuLexer_drop(p->lexer);
    FuMem_free(p);
}

void FuParser_for_file(FuParser *p, char *fpath, fu_size_t len) {
    p->lexer = FuLexer_new(p->ctx);
    FuLexer_for_file(p->lexer, fpath, len);
}

static FuToken FuParser_get_token(FuParser *p) {
    FuToken tok0, tok1, tok2;
    FuSpan span;

    tok0 = FuLexer_get_token(p->lexer);
    if (p->in_tok_tree) {
        return tok0;
    }

    switch (tok0.kd) {
    case TOK_COMMENT: {
        /* ignore comment */
        do {
            tok0 = FuLexer_get_token(p->lexer);
        } while (tok0.kd == TOK_COMMENT);
        FuLexer_unget_token(p->lexer, tok0);
        return FuToken_new(TOK_NEWLINE, tok0.span);
        break;
    }
    case TOK_RAW_IDENT:
        return FuToken_new_ident(tok0.span, tok0.sym);
        break;
    case TOK_IDENT: {
        if (tok0.sym < _KW_LAST_UNUSED) {
            return FuToken_new_keyword(tok0.span, tok0.sym);
        } else {
            return tok0;
        }
        break;
    }
    case TOK_PLUS: {
        /* `+`, `+=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `+=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_PLUS_EQ, span);
        } else {
            /* `+` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_MINUS: {
        /* `-`, `-=`, `->` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `-=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_MINUS_EQ, span);
        } else if (tok1.kd == TOK_GT) {
            /* `->` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_RARROW, span);
        } else {
            /* `-` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_STAR: {
        /* `*`, `*=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `*=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_STAR_EQ, span);
        } else {
            /* `*` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_SLASH: {
        /* `/`, `/=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `/=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_SLASH_EQ, span);
        } else {
            /* `/` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_PERCENT: {
        /* `%`, `%=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `%=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_PERCENT_EQ, span);
        } else {
            /* `%` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_AND: {
        /* `&`, `&=`, `&&` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `&=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_AND_EQ, span);
        } else if (tok1.kd == TOK_AND) {
            /* `&&` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_AND_AND, span);
        } else {
            /* `&` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_OR: {
        /* `|`, `|=`, `||` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `|=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_OR_EQ, span);
        } else if (tok1.kd == TOK_OR) {
            /* `||` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_OR_OR, span);
        } else {
            /* `|` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_CARET: {
        /* `^`, `^=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `^=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_CARET_EQ, span);
        } else {
            /* `^` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_LT: {
        /* `<`, `<<`, `<<=`, `<=`, `<-` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_LT) {
            /* `<<`, `<<=` */
            span = FuSpan_join(tok0.span, tok1.span);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_EQ) {
                /* `<<=` */
                span = FuSpan_join(span, tok2.span);
                return FuToken_new(TOK_SHL_EQ, span);
            } else {
                /* `<<` */
                FuLexer_unget_token(p->lexer, tok2);
                return FuToken_new(TOK_SHL, span);
            }
        } else if (tok1.kd == TOK_EQ) {
            /* `<=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_LE, span);
        } else if (tok1.kd == TOK_MINUS) {
            /* `<-` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_LARROW, span);
        } else {
            /* `<` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_GT: {
        /* `>`, `>>` `>>=`, `>=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_GT) {
            /* `>>`, `>>=` */
            span = FuSpan_join(tok0.span, tok1.span);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_EQ) {
                /* `>>=` */
                span = FuSpan_join(span, tok2.span);
                return FuToken_new(TOK_SHR_EQ, span);
            } else {
                /* `>>` */
                FuLexer_unget_token(p->lexer, tok2);
                return FuToken_new(TOK_SHR, span);
            }
        } else if (tok1.kd == TOK_EQ) {
            /* `>=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_GE, span);
        } else {
            /* `>` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_EQ: {
        /* `=`, `==`, `=>` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `==` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_EE, span);
        } else if (tok1.kd == TOK_GT) {
            /* `=>` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_FAT_ARROW, span);
        } else {
            /* `=` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_NOT: {
        /* `!`, `!=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `!=` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_NE, span);
        } else {
            /* `!` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_DOT: {
        /* `.`, `..`, `...`, `..=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_DOT) {
            /* `..`, `...`, `..=` */
            span = FuSpan_join(tok0.span, tok1.span);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_DOT) {
                /* `...` */
                span = FuSpan_join(span, tok2.span);
                return FuToken_new(TOK_DOT_DOT_DOT, span);
            } else if (tok2.kd == TOK_EQ) {
                /* `..=` */
                span = FuSpan_join(span, tok2.span);
                return FuToken_new(TOK_DOT_DOT_EQ, span);
            } else {
                /* `..` */
                FuLexer_unget_token(p->lexer, tok2);
                return FuToken_new(TOK_DOT_DOT, span);
            }
        } else {
            /* `.` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_COLON: {
        /* `:`, `::` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_COLON) {
            /* `::` */
            span = FuSpan_join(tok0.span, tok1.span);
            return FuToken_new(TOK_MOD_SEP, span);
        } else {
            /* `:` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    default:
        return tok0;
        break;
    }
}

static FuToken FuParser_nth_token(FuParser *p, fu_size_t n) {
    fu_size_t i = p->cursor + n;
    while (i >= FuVec_len(p->tok_buf)) {
        FuToken tok = FuParser_get_token(p);
        FuVec_push(p->tok_buf, &tok);
    }
    return *(FuToken *)FuVec_get(p->tok_buf, i);
}

static fu_bool_t FuParser_is_eof(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_EOF) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static FuToken FuParser_bump(FuParser *p) {
    FuToken cur_tok = FuParser_nth_token(p, 0);
    FuVec_remove_slice(p->tok_buf, 0, p->cursor + 1, NULL);
    FuToken tok = FuParser_get_token(p);
    FuVec_push(p->tok_buf, &tok);
    return cur_tok;
}

static FuSpan FuParser_current_span(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    return tok.span;
}

static FuToken FuParser_expect_keyword(FuParser *p, fu_keyword_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        FATAL(&tok.span, "expect %s, find token: %s", FuKind_keyword_cstr(kd), FuKind_token_cstr(tok.kd));
    }
    if (tok.sym != kd) {
        FATAL(&tok.span, "expect %s, find keyword: %s", FuKind_keyword_cstr(kd), FuKind_keyword_cstr(tok.sym));
    }
    return FuParser_bump(p);
}

static FuToken FuParser_expect_token(FuParser *p, fu_token_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != kd) {
        FATAL(&tok.span, "expect token: %s, find token: %s", FuKind_token_cstr(kd), FuKind_token_cstr(tok.kd));
    }
    return FuParser_bump(p);
}

/*
static fu_bool_t FuParser_check_2_sep_token(FuParser *p, fu_token_k kind0, fu_token_k kind1) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    FuToken tok1;
    fu_size_t i = 1;
    do {
        tok1 = FuParser_nth_token(p, i);
        i++;
    } while (tok1.kd == TOK_WHITESPACE);
    if (tok0.kd == kind0 && tok1.kd == kind1) {
        return FU_TRUE;
    }
    return FU_FALSE;
}
*/

static FuToken FuParser_expect_token_fn(FuParser *p, FuCheckTokenFn fn, char *wanted) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!fn(tok)) {
        FATAL(&tok.span, "expect %s, find token: %s", wanted, FuKind_token_cstr(tok.kd));
    }
    return FuParser_bump(p);
}

static fu_bool_t FuParser_check_token(FuParser *p, fu_token_k kd) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == kd) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_token_fn(FuParser *p, FuCheckTokenFn fn) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (fn(tok)) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static void FuParser_eat_whitespace(FuParser *p, fu_bool_t must_has) {
    FuSpan start = FuParser_current_span(p);
    fu_bool_t has_whitespace = FU_FALSE;
    while (1) {
        if (!FuParser_check_token(p, TOK_WHITESPACE)) {
            break;
        }
        has_whitespace = FU_TRUE;
        FuParser_bump(p);
    }
    if (must_has && !has_whitespace) {
        FATAL(&start, "expect whitespace");
    }
}

static fu_bool_t FuParser_eat_blank(FuParser *p) {
    fu_bool_t has_newline = FU_FALSE;
    while (1) {
        if (!FuParser_check_token_fn(p, FuToken_is_blank)) {
            break;
        }
        FuToken tok = FuParser_bump(p);
        if (tok.kd == TOK_NEWLINE) {
            has_newline = FU_TRUE;
        }
    }
    return has_newline;
}

FuLit *FuToken_to_lit_nil(FuToken tok) {
    assert(tok.kd == TOK_KEYWORD);
    assert(tok.sym == KW_NIL);
    return FuLit_new(tok.span, LIT_NIL);
}

FuLit *FuToken_to_lit_bool(FuToken tok) {
    assert(tok.kd == TOK_KEYWORD);
    assert(tok.sym == KW_TRUE || tok.sym == KW_FALSE);
    FuLit *lit = FuLit_new(tok.span, LIT_BOOL);
    if (tok.sym == KW_TRUE) {
        lit->_bool.v = 1;
    }
    if (tok.sym == KW_FALSE) {
        lit->_bool.v = 0;
    }
    return lit;
}

fu_uint8_t Fu_decode_escape_byte(FuSpan span, FuStr *str, fu_size_t start, fu_size_t *offset, fu_bool_t check_len) {
    FuSpan err_span;
    /* start with backslash `\` */
    assert(FuStr_get_char(str, start) == '\\');
    fu_size_t str_len = FuStr_len(str);
    fu_size_t left_len = str_len - start;
    if (left_len < 2) {
        err_span = FuSpan_offset(span, *offset);
        FATAL(&err_span, "invalid byte escape sequences");
    }
    FuChar flag_char = FuStr_get_char(str, start + 1);
    *offset += 1;
/* clang-format off */
#define CHECK_STR_LEN(len) \
    if (check_len && str_len > len) { \
        err_span = FuSpan_offset(span, *offset+1); \
        FATAL(&err_span, "invalid extra byte escape sequences"); \
    }
    /* clang-format on */
    switch (flag_char) {
    case '0':
        CHECK_STR_LEN(2);
        return '\0';
        break;
    case 'a':
        CHECK_STR_LEN(2);
        return '\a';
        break;
    case 'b':
        CHECK_STR_LEN(2);
        return '\b';
        break;
    case 'f':
        CHECK_STR_LEN(2);
        return '\f';
        break;
    case 'n':
        CHECK_STR_LEN(2);
        return '\n';
        break;
    case 'r':
        CHECK_STR_LEN(2);
        return '\r';
        break;
    case 't':
        CHECK_STR_LEN(2);
        return '\t';
        break;
    case 'v':
        CHECK_STR_LEN(2);
        return '\v';
        break;
    case '\'':
        CHECK_STR_LEN(2);
        return '\'';
        break;
    case '\"':
        CHECK_STR_LEN(2);
        return '\"';
        break;
    case '\\':
        CHECK_STR_LEN(2);
        return '\\';
        break;
    case 'x':
        /* '\xff' */
        if (left_len < 4) {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "byte escape sequences must be followed by 2 hex digits");
        }
        fu_uint8_t v = 0;
        fu_size_t i;
        for (i = 2; i < 4; i++) {
            FuChar bit = FuStr_get_char(str, start + i);
            if (bit == '\n') {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "byte escape sequences must be followed by 2 hex digits in the same line");
            }
            *offset += 1;
            if (!(FuChar_is_lowercase_hexadecimal_digit(bit))) {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "byte escape sequences must be lowercase hex digit");
            }
            v = v * 16 + FuChar_digit_to_uint(bit, 16);
        }
        CHECK_STR_LEN(4);
        return v;
        break;
    default:
        err_span = FuSpan_offset(span, *offset);
        FATAL(&err_span, "unknown byte escape sequences");
    }
#undef CHECK_STR_LEN
    /* can not be here */
    return 0;
}

FuLit *FuToken_to_lit_byte(FuToken tok) {
    assert(tok.kd == TOK_BYTE);
    if (!tok._byte.terminated) {
        FATAL(&tok.span, "mismatched char closing delimiter");
    }

    FuLit *lit = FuLit_new(tok.span, LIT_BYTE);
    FuStr *symbol = FuContext_get_symbol(tok.span.ctx, tok._byte.sym);
    fu_size_t len = FuStr_len(symbol);
    if (len == 0) {
        FATAL(&tok.span, "empty byte");
    }
    FuChar first_char = FuStr_get_char(symbol, 0);
    if (first_char != '\\') {
        /* 'a' */
        if (FuStr_len(symbol) != 1) {
            FATAL(&tok.span, "byte must has one character");
        }
        if (!FuChar_is_ascii(first_char)) {
            FATAL(&tok.span, "byte must be ascii");
        }
        lit->_byte.v = first_char;
    } else {
        /* escape byte b'\n', b'\xff' */
        fu_size_t offset = 1;
        lit->_byte.v = Fu_decode_escape_byte(tok.span, symbol, 0, &offset, FU_TRUE);
    }
    return lit;
}

FuLit *FuToken_to_lit_byte_str(FuToken tok) {
    FuSpan err_span;
    assert(tok.kd == TOK_BYTE_STR || tok.kd == TOK_BYTE_RAW_STR);
    if (!tok._str.started) {
        FATAL(&tok.span, "byte str needed open delimiter");
    }
    if (!tok._str.terminated) {
        FATAL(&tok.span, "mismatched byte str closing delimiter");
    }
    FuLit *lit = FuLit_new(tok.span, LIT_BYTE_STR);
    FuStr *symbol = FuContext_get_symbol(tok.span.ctx, tok._str.sym);
    FuStr *fcontent = FuContext_get_file(tok.span.ctx, tok.span.fpath);

    FuBytes *bytes = FuBytes_new();
    fu_size_t len = FuStr_len(symbol);

    FuChar fc;
    fu_size_t i = 0;
    fu_size_t offset = FuToken_left_skip_count(tok);
    if (tok._str.prefix_ignore) {
        FuChar fc = FuStr_get_char(fcontent, tok.span.start + offset);
        if (fc != '\n') {
            err_span = FuSpan_offset(tok.span, offset);
            FATAL(&err_span, "byte str must started with newline");
        }
        offset += 1 + tok._str.prefix_ignore;
    }
    for (; i < len; i++) {
        if (tok.kd == TOK_BYTE_RAW_STR) {
            fc = FuStr_get_char(symbol, i);
            offset++;
            if (!FuChar_is_ascii(fc)) {
                err_span = FuSpan_offset(tok.span, offset);
                FATAL(&err_span, "byte must be ascii");
            }
            FuBytes_push(bytes, fc);
            if (fc == '\n') {
                offset += tok._str.prefix_ignore;
            }
            continue;
        }
        /* TOK_BYTE_STR */
        fc = FuStr_get_char(symbol, i);
        if (fc != '\\') {
            if (!FuChar_is_ascii(fc)) {
                err_span = FuSpan_offset(tok.span, offset);
                FATAL(&err_span, "byte must be ascii");
            }
            offset++;
            FuBytes_push(bytes, fc);
            if (fc == '\n') {
                offset += tok._str.prefix_ignore;
            }
            continue;
        }
        /* \newline, \a, \n, \f, ...,  \xff */
        fu_size_t next_i = i + 1;
        if (next_i < len) {
            fc = FuStr_get_char(symbol, next_i);
            if (fc == '\n') {
                /* 如果一行以 \ 结束，那么会忽略这行的换行符 */
                i++;
                offset += 1 + 1 + tok._str.prefix_ignore;
                continue;
            }
        }
        fu_size_t old_offset = offset;
        fu_uint8_t byte = Fu_decode_escape_byte(tok.span, symbol, i, &offset, FU_FALSE);
        FuBytes_push(bytes, byte);
        i += offset - old_offset - 1;
    }
    lit->_byte_str = bytes;
    return lit;
}

FuChar Fu_decode_escape_char(FuSpan span, FuStr *str, fu_size_t start, fu_size_t *offset, fu_bool_t check_len) {
    FuSpan err_span;
    /* start with backslash `\` */
    assert(FuStr_get_char(str, start) == '\\');
    fu_size_t str_len = FuStr_len(str);
    fu_size_t left_len = str_len - start;
    if (left_len < 2) {
        err_span = FuSpan_offset(span, *offset);
        FATAL(&err_span, "invalid char escape sequences");
    }
    FuChar flag_char = FuStr_get_char(str, start + 1);
    *offset += 1;
/* clang-format off */
#define CHECK_STR_LEN(len) \
    if (check_len && str_len > len) { \
        err_span = FuSpan_offset(span, *offset + 1); \
        FATAL(&err_span, "invalid extra char escape sequences"); \
    }
    /* clang-format on */
    fu_size_t i;
    switch (flag_char) {
    case '0':
        CHECK_STR_LEN(2);
        return '\0';
        break;
    case 'a':
        CHECK_STR_LEN(2);
        return '\a';
        break;
    case 'b':
        CHECK_STR_LEN(2);
        return '\b';
        break;
    case 'f':
        CHECK_STR_LEN(2);
        return '\f';
        break;
    case 'n':
        CHECK_STR_LEN(2);
        return '\n';
        break;
    case 'r':
        CHECK_STR_LEN(2);
        return '\r';
        break;
    case 't':
        CHECK_STR_LEN(2);
        return '\t';
        break;
    case 'v':
        CHECK_STR_LEN(2);
        return '\v';
        break;
    case '\'':
        CHECK_STR_LEN(2);
        return '\'';
        break;
    case '\"':
        CHECK_STR_LEN(2);
        return '\"';
        break;
    case '\\':
        CHECK_STR_LEN(2);
        return '\\';
        break;
    case 'x':
        /* '\xff' */
        if (left_len < 4) {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "char escape sequences must be followed by 2 hex digits");
        }
        fu_uint8_t xv = 0;
        for (i = 2; i < 4; i++) {
            FuChar bit = FuStr_get_char(str, start + i);
            if (bit == '\n') {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "char escape sequences must be followed by 2 hex digits in the same line");
            }
            *offset += 1;
            if (!(FuChar_is_lowercase_hexadecimal_digit(bit))) {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "char escape sequences must be lowercase hex digit");
            }
            xv = xv * 16 + FuChar_digit_to_uint(bit, 16);
        }
        CHECK_STR_LEN(4);
        return xv;
        break;
    case 'u':
        /* '\u{a.....}' */
        if (left_len > 2 && FuStr_get_char(str, start + 2) == '\n') {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "char escape sequences must enclosed by braces: `\\u{ff}` in the same line");
        }
        if (left_len > 2 && FuStr_get_char(str, start + 2) != '{') {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "char escape sequences must use braces: `\\u{ff}`");
        }
        if (left_len < 5) {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "char escape sequences must has 1-6 hex digits enclosed by braces: `\\u{ff}`");
        }
        fu_uint32_t uv = 0;
        for (i = 3; i < 9; i++) {
            FuChar bit = FuStr_get_char(str, i);
            *offset += 1;
            if (bit == '}') {
                break;
            }
            if (bit == '\n') {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "char escape sequences must be followed by 2 hex digits in the same line");
            }
            if (!FuChar_is_lowercase_hexadecimal_digit(bit)) {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "char escape sequences must be lowercase hex digit");
            }
            uv = uv * 16 + FuChar_digit_to_uint(bit, 16);
        }
        if (i == 9) {
            FuChar bit = FuStr_get_char(str, i);
            if (bit != '}') {
                err_span = FuSpan_offset(span, *offset);
                FATAL(&err_span, "char escape sequences must has 1-6 hex digits enclosed by braces: `\\u{ff}`");
            }
        }
        if (uv > 0x10FFFF) {
            err_span = FuSpan_offset(span, *offset);
            FATAL(&err_span, "invalid codepoint for \\u escape sequence");
        }
        return uv;
    default:
        err_span = FuSpan_offset(span, *offset);
        FATAL(&err_span, "unknown byte escape sequences");
    }
#undef CHECK_STR_LEN
    /* can not be here */
    return 0;
}

FuLit *FuToken_to_lit_char(FuToken tok) {
    assert(tok.kd == TOK_CHAR);
    if (!tok._char.terminated) {
        FATAL(&tok.span, "mismatched char closing delimiter");
    }

    FuLit *lit = FuLit_new(tok.span, LIT_CHAR);
    FuStr *symbol = FuContext_get_symbol(tok.span.ctx, tok._char.sym);
    fu_size_t len = FuStr_len(symbol);
    if (len == 0) {
        FATAL(&tok.span, "empty char");
    }
    FuChar first_char = FuStr_get_char(symbol, 0);
    if (first_char != '\\') {
        /* 'a' */
        if (FuStr_len(symbol) != 1) {
            FATAL(&tok.span, "char must has one character");
        }
        lit->_char.v = first_char;
        return lit;
    } else {
        /* escape byte '\n', '\xff', '\u{ff}' */
        fu_size_t offset = 0;
        lit->_byte.v = Fu_decode_escape_char(tok.span, symbol, 0, &offset, FU_TRUE);
    }
    return lit;
}

FuLit *FuToken_to_lit_str(FuToken tok) {
    FuSpan err_span;
    assert(tok.kd == TOK_STR || tok.kd == TOK_RAW_STR);
    if (!tok._str.started) {
        FATAL(&tok.span, "str needed open delimiter");
    }
    if (!tok._str.terminated) {
        FATAL(&tok.span, "mismatched str closing delimiter");
    }
    FuLit *lit = FuLit_new(tok.span, LIT_STR);
    FuStr *symbol = FuContext_get_symbol(tok.span.ctx, tok._str.sym);
    FuStr *fcontent = FuContext_get_file(tok.span.ctx, tok.span.fpath);

    FuStr *str = FuStr_new();
    fu_size_t len = FuStr_len(symbol);

    FuChar fc;
    fu_size_t i = 0;
    fu_size_t offset = FuToken_left_skip_count(tok);
    if (tok._str.prefix_ignore) {
        fc = FuStr_get_char(fcontent, tok.span.start + offset);
        if (fc != '\n') {
            err_span = FuSpan_offset(tok.span, offset);
            FATAL(&err_span, "str must started with newline");
        }
        offset += 1 + tok._str.prefix_ignore;
    }
    for (; i < len; i++) {
        if (tok.kd == TOK_RAW_STR) {
            fc = FuStr_get_char(symbol, i);
            offset++;
            FuStr_push(str, fc);
            if (fc == '\n') {
                offset += tok._str.prefix_ignore;
            }
            continue;
        }
        /* TOK_BYTE_STR */
        fc = FuStr_get_char(symbol, i);
        if (fc != '\\') {
            offset++;
            FuStr_push(str, fc);
            if (fc == '\n') {
                offset += tok._str.prefix_ignore;
            }
            continue;
        }
        /* \newline, \a, \n, \f, ...,  \xff */
        fu_size_t next_i = i + 1;
        if (next_i < len) {
            fc = FuStr_get_char(symbol, next_i);
            if (fc == '\n') {
                /* 如果一行以 \ 结束，那么会忽略这行的换行符 */
                i++;
                offset += 1 + 1 + tok._str.prefix_ignore;
                continue;
            }
        }
        fu_size_t old_offset = offset;
        FuChar c = Fu_decode_escape_char(tok.span, symbol, i, &offset, FU_FALSE);
        FuStr_push(str, c);
        i += offset - old_offset - 1;
    }
    lit->_str = str;
    return lit;
}

fu_uint64_t Fu_cstr_to_uint64(FuSpan span, char *cstr, fu_size_t base) {
    if (base < 2 || base > 36) {
        FATAL(&span, "invalid int base");
    }
    fu_uint64_t v, old_v;
    v = old_v = 0;
    char *p = cstr;
    while (*p) {
        old_v = v;
        v = v * base;
        if (v / base != old_v) {
            FATAL(&span, "too big int");
        }
        old_v = v;
        v += FuChar_digit_to_uint(*p, base);
        if (old_v > v) {
            FATAL(&span, "too big int");
        }
        p++;
    }
    return v;
}

fu_err_t Fu_check_int_suffix(FuLit *lit, FuStr *suffix) {
    if (FuStr_eq_cstr(suffix, "i8")) {
        lit->_int.size = 8;
        lit->_int.is_signed = 1;
    } else if (FuStr_eq_cstr(suffix, "u8")) {
        lit->_int.size = 8;
        lit->_int.is_signed = 0;
    } else if (FuStr_eq_cstr(suffix, "i16")) {
        lit->_int.size = 16;
        lit->_int.is_signed = 1;
    } else if (FuStr_eq_cstr(suffix, "u16")) {
        lit->_int.size = 16;
        lit->_int.is_signed = 0;
    } else if (FuStr_eq_cstr(suffix, "i32")) {
        lit->_int.size = 32;
        lit->_int.is_signed = 1;
    } else if (FuStr_eq_cstr(suffix, "u32")) {
        lit->_int.size = 32;
        lit->_int.is_signed = 0;
    } else if (FuStr_eq_cstr(suffix, "i64")) {
        lit->_int.size = 64;
        lit->_int.is_signed = 1;
    } else if (FuStr_eq_cstr(suffix, "u64")) {
        lit->_int.size = 64;
        lit->_int.is_signed = 0;
    } else if (FuStr_eq_cstr(suffix, "")) {
        /* unsuffix */
        lit->_int.size = 0;
        lit->_int.is_signed = 0;
    } else {
        return 1;
    }
    return 0;
}

FuLit *FuToken_to_lit_int(FuToken tok) {
    FuSpan err_span;
    assert(tok.kd == TOK_INT);
    FuLit *lit = FuLit_new(tok.span, LIT_INT);
    FuStr *symbol = FuContext_get_symbol(tok.span.ctx, tok._str.sym);
    fu_size_t len = FuStr_len(symbol);

    if (tok._int.empty_int) {
        FATAL(&tok.span, "empty int");
    }

    /* remove `_` */
    char buf[2048];
    fu_size_t nbuf = 0;
    fu_size_t i = 0;
    if (tok._int.base == 2 || tok._int.base == 8 || tok._int.base == 16) {
        i += 2;
    }

    for (; i < tok._int.suffix_start; i++) {
        FuChar fc = FuStr_get_char(symbol, i);
        if (fc == '_') {
            continue;
        }
        /* clang-format off */
#define ERR_DIGIT(kind) \
    err_span = FuSpan_offset(tok.span, i); \
    FATAL(&err_span, "invalid " kind " digit");
        /* clang-format on */
        switch (tok._int.base) {
        case 2:
            if (!FuChar_is_binary_digit(fc)) {
                ERR_DIGIT("binary");
            }
            break;
        case 8:
            if (!FuChar_is_octal_digit(fc)) {
                ERR_DIGIT("octal");
            }
            break;
        case 10:
            if (!FuChar_is_decimal_digit(fc)) {
                ERR_DIGIT("decimal");
            }
            break;
        case 16:
            if (!FuChar_is_hexadecimal_digit(fc)) {
                ERR_DIGIT("hexadecimal");
            }
            break;
        default:
            break;
        }
#undef ERR_DIGIT

        if (nbuf > 2047) {
            FATAL(&tok.span, "too big int");
        }
        buf[nbuf] = fc;
        nbuf++;
    }
    buf[nbuf] = 0;
    lit->_int.v = Fu_cstr_to_uint64(tok.span, buf, tok._int.base);
    FuStr *suffix = FuStr_from_slice(symbol, tok._int.suffix_start, len);
    if (Fu_check_int_suffix(lit, suffix)) {
        err_span = FuSpan_offset(tok.span, tok._int.suffix_start);
        FATAL(&err_span, "invalid int suffix");
    }
    FuStr_drop(suffix);
    return lit;
}

FuLit *FuToken_to_lit_float(FuToken tok) {
    FuLit *lit = FuLit_new(tok.span, LIT_FLOAT);
    /* todo */
    return lit;
}

FuLit *FuToken_to_lit_format_str(FuToken tok) {
    FuLit *lit = FuLit_new(tok.span, LIT_FORMAT_STR);
    /* todo */
    return lit;
}

FuLit *FuParser_parse_lit(FuParser *p) {
    FuToken tok = FuParser_expect_token_fn(p, FuToken_is_lit, "literal");
    FuLit *lit = NULL;
    switch (tok.kd) {
    case TOK_KEYWORD:
        if (tok.sym == KW_NIL) {
            lit = FuToken_to_lit_nil(tok);
        } else if (tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
            lit = FuToken_to_lit_bool(tok);
        } else {
            FATAL(&tok.span, "expect literal");
        }
        break;
    case TOK_BYTE:
        lit = FuToken_to_lit_byte(tok);
        break;
    case TOK_CHAR:
        lit = FuToken_to_lit_char(tok);
        break;
    case TOK_INT:
        lit = FuToken_to_lit_int(tok);
        break;
    case TOK_FLOAT:
        lit = FuToken_to_lit_float(tok);
        break;
    case TOK_STR:
    case TOK_RAW_STR:
        lit = FuToken_to_lit_str(tok);
        break;
    case TOK_BYTE_STR:
    case TOK_BYTE_RAW_STR:
        lit = FuToken_to_lit_byte_str(tok);
        break;
    case TOK_FORMAT_STR:
    case TOK_FORMAT_RAW_STR:
        FATAL(&tok.span, "unimplemented: %s", FuKind_token_cstr(tok.kd));
        break;
    default:
        FATAL(&tok.span, "can not be here");
    }
    return lit;
}

FuExpr *FuParser_parse_expr(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    FuExpr *expr;
    switch (tok.kd) {
    case TOK_BYTE:
    case TOK_CHAR:
    case TOK_INT:
    case TOK_FLOAT:
    case TOK_STR:
    case TOK_RAW_STR:
    case TOK_BYTE_STR:
    case TOK_BYTE_RAW_STR:
    case TOK_FORMAT_STR:
    case TOK_FORMAT_RAW_STR:
    case TOK_KEYWORD: {
        if (FuToken_is_lit(tok)) {
            FuLit *lit = FuParser_parse_lit(p);
            expr = FuExpr_new_lit(lit);
        }
        break;
    }
    case TOK_IDENT: {
        /* todo: expr->_path.anno */
        FuPath *path = FuParser_parse_path(p);
        expr = FuExpr_new_path(NULL, path);
        break;
    }
    default:
        FATAL(NULL, "unimplemented: %s", FuKind_token_cstr(tok.kd));
    }
    return expr;
}

/*
  关键字 nil, true, false 的 kd 属于 TOK_IDENT，
  这样在宏里面这些关键字可以统一作为 TOK_IDENT 类型
 */
FuIdent *FuParser_parse_ident(FuParser *p) {
    FuSpan span = FuParser_current_span(p);
    FuToken tok = FuParser_expect_token_fn(p, FuToken_is_ident, "expect ident");
    FuIdent *ident = FuMem_new(FuIdent);
    ident->span = span;
    ident->name = tok.sym;
    return ident;
}

FuPathItem *FuParser_parse_path_item(FuParser *p) {
    FuSpan lo = FuParser_current_span(p);
    FuIdent *ident = FuParser_parse_ident(p);
    FuVec *ge_args = NULL;
    /* todo: generic
    if (FuParser_check_2_token(p, TOK_POUND, TOK_LT)) {
        ge_args = FuParser_parse_ge_args(p);
    }
    */
    FuPathItem *item = FuMem_new(FuPathItem);
    item->span = FuSpan_join(lo, FuParser_current_span(p));
    item->ident = ident;
    item->ge_args = ge_args;
    return item;
}

/* todo: generic */
FuPath *FuParser_parse_path(FuParser *p) {
    FuPath *path = FuMem_new(FuPath);
    path->segments = FuVec_new(sizeof(FuPathItem *));
    while (1) {
        FuPathItem *item = FuParser_parse_path_item(p);
        FuVec_push_ptr(path->segments, item);
        if (FuParser_check_token(p, TOK_MOD_SEP)) {
            FuParser_bump(p);
            FuParser_bump(p);
        } else {
            break;
        }
    }
    FuPathItem *start = FuVec_first_ptr(path->segments);
    FuPathItem *end = FuVec_last_ptr(path->segments);
    path->span = FuSpan_join(start->span, end->span);
    return path;
}

fu_vis_k FuParser_parse_visibility(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return VIS_PRI;
    }
    FuParser_bump(p);
    switch (tok.sym) {
    case KW_PUB:
        return VIS_PUB;
        break;
    case KW_PKG:
        return VIS_PKG;
        break;
    default:
        return VIS_PRI;
        break;
    }
}

FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_STATIC);
    FuParser_eat_whitespace(p, FU_TRUE);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_eat_whitespace(p, FU_FALSE);
    FuParser_expect_token(p, TOK_EQ);
    FuParser_eat_whitespace(p, FU_FALSE);
    FuExpr *expr = FuParser_parse_expr(p);
    FuSpan span = FuSpan_join(lo, expr->span);
    FuNode *nd = FuNode_new(p->ctx, span, ND_STATIC);
    nd->attrs = attrs;
    nd->_static.vis = vis;
    nd->_static.ident = ident;
    nd->_static.init_expr = expr;
    return nd;
}

FuNode *FuParser_parse_mod_item(FuParser *p) {
    /* todo: parse attrs */
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    fu_vis_k vis = FuParser_parse_visibility(p);
    FuParser_eat_whitespace(p, FU_FALSE);
    FuToken tok = FuParser_nth_token(p, 0);
    FuNode *item;
    if (tok.kd != TOK_KEYWORD) {
        /* todo: attr drop */
        FuVec_drop(attrs);
        FATAL(&tok.span, "expect item keyword, find tok: %s", FuKind_token_cstr(tok.kd));
    }
    switch (tok.sym) {
    case KW_STATIC:
        item = FuParser_parse_item_static(p, attrs, vis);
        break;
    default:
        /* todo: attr drop */
        FuVec_drop(attrs);
        FATAL(&tok.span, "unimplement item: %s", FuKind_keyword_cstr(tok.sym));
        break;
    }
    return item;
}

FuVec *FuParser_parse_mod_items(FuParser *p) {
    FuVec *items = FuVec_new(sizeof(FuNode *));
    FuParser_eat_blank(p);
    while (!FuParser_is_eof(p)) {
        FuNode *item = FuParser_parse_mod_item(p);
        FuVec_push_ptr(items, item);
        FuParser_eat_blank(p);
    }
    return items;
}

FuNode *FuParser_parse_pkg(FuParser *p) {
    FuParser_eat_blank(p);
    FuSpan lo = FuParser_current_span(p);
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* FuParser_parse_inner_attrs(p, attrs); */
    FuVec *items = FuParser_parse_mod_items(p);
    FuSpan hi = FuParser_current_span(p);
    FuSpan span = FuSpan_join(lo, hi);
    FuNode *nd = FuNode_new_pkg(p->ctx, span);
    nd->attrs = attrs;
    nd->_pkg.name = KW_DOLLAR_PKG;
    nd->_pkg.items = items;
    return nd;
}

FuStr *FuParser_dump_tokens(FuParser *p) {
    FuStr *dump = FuStr_new();
    while (!FuParser_is_eof(p)) {
        FuToken tok = FuParser_nth_token(p, 0);
        FuStr *tok_str = FuToken_display(tok);
        FuStr_append(dump, FuSpan_display(tok.span));
        FuStr_push_utf8_cstr(dump, ":");
        FuStr_append(dump, tok_str);
        FuStr_push_utf8_cstr(dump, "\n");
        FuParser_bump(p);
    }
    return dump;
}
