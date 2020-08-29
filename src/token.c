#include <assert.h>

#include "log.h"
#include "parse.h"

FuToken FuToken_new(fu_token_k kd, FuSpan *sp) {
    assert(kd < TOK_DOC_COMMENT);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    return token;
}

static FuToken FuToken_new_sym(fu_token_k kd, FuSpan *sp, fu_sym_t sym) {
    assert(kd >= TOK_DOC_COMMENT && kd < TOK_BYTE);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    token.sym = sym;
    return token;
}

FuToken FuToken_new_doc_comment(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_DOC_COMMENT, sp, sym);
}

FuToken FuToken_new_keyword(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_KEYWORD, sp, sym);
}

FuToken FuToken_new_ident(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_IDENT, sp, sym);
}

FuToken FuToken_new_raw_ident(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_RAW_IDENT, sp, sym);
}

FuToken FuToken_new_macro(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_MACRO, sp, sym);
}

FuToken FuToken_new_lable(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_LABEL, sp, sym);
}

FuToken FuToken_new_lit_int(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_int, fu_size_t suffix_start) {
    FuToken token;
    token.kd = TOK_INT;
    token.sp = sp;
    token._int.sym = sym;
    token._int.base = base;
    token._int.empty_int = empty_int;
    token._int.suffix_start = suffix_start;
    return token;
}

FuToken FuToken_new_lit_float(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_exponent,
                              fu_size_t suffix_start) {
    FuToken token;
    token.kd = TOK_FLOAT;
    token.sp = sp;
    token._float.sym = sym;
    token._float.base = base;
    token._float.empty_exponent = empty_exponent;
    token._float.suffix_start = suffix_start;
    return token;
}

FuToken FuToken_new_lit_char(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated) {
    FuToken token;
    token.kd = TOK_CHAR;
    token.sp = sp;
    token._char.sym = sym;
    token._char.terminated = terminated;
    return token;
}

FuToken FuToken_new_lit_byte(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated) {
    FuToken token;
    token.kd = TOK_BYTE;
    token.sp = sp;
    token._byte.sym = sym;
    token._byte.terminated = terminated;
    return token;
}

FuToken FuToken_new_lit_str(fu_token_k kd, FuSpan *sp, fu_sym_t sym, fu_size_t n_hashes, fu_bool_t started,
                            fu_size_t prefix_ignore, fu_bool_t terminated) {
    assert(TOK_STR <= kd && kd <= TOK_FORMAT_RAW_STR);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    token._str.sym = sym;
    token._str.n_hashes = n_hashes;
    token._str.started = started;
    token._str.prefix_ignore = prefix_ignore;
    token._str.terminated = terminated;
    return token;
}

fu_bool_t FuToken_is_eof(FuToken tok) {
    return tok.kd == TOK_EOF;
}

fu_bool_t FuToken_is_ident(FuToken tok) {
    if (tok.kd == TOK_RAW_IDENT) {
        return FU_TRUE;
    }
    if (tok.kd == TOK_IDENT && tok.sym >= _KW_LAST_UNUSED) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_keyword(FuToken tok) {
    if (tok.kd == TOK_IDENT && tok.sym < _KW_LAST_UNUSED) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_open_delim(FuToken tok) {
    switch (tok.kd) {
    case TOK_OPEN_PAREN:
    case TOK_OPEN_BRACKET:
    case TOK_OPEN_BRACE:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_close_delim(FuToken tok) {
    switch (tok.kd) {
    case TOK_CLOSE_PAREN:
    case TOK_CLOSE_BRACKET:
    case TOK_CLOSE_BRACE:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_match_delim(FuToken open, FuToken close) {
    if (open.kd == TOK_OPEN_PAREN && close.kd == TOK_CLOSE_PAREN) {
        return FU_TRUE;
    }
    if (open.kd == TOK_OPEN_BRACKET && close.kd == TOK_CLOSE_BRACKET) {
        return FU_TRUE;
    }
    if (open.kd == TOK_OPEN_BRACE && close.kd == TOK_CLOSE_BRACE) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_assign(FuToken tok) {
    switch (tok.kd) {
    case TOK_EQ:
    case TOK_PLUS_EQ:
    case TOK_MINUS_EQ:
    case TOK_STAR_EQ:
    case TOK_SLASH_EQ:
    case TOK_PERCENT_EQ:
    case TOK_AND_EQ:
    case TOK_OR_EQ:
    case TOK_CARET_EQ:
    case TOK_SHL_EQ:
    case TOK_SHR_EQ:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_lit(FuToken tok) {
    if (TOK_BYTE <= tok.kd && tok.kd <= TOK_FORMAT_RAW_STR) {
        return FU_TRUE;
    }
    if (tok.kd == TOK_KEYWORD) {
        /* ident: nil, true, false */
        if (tok.sym == KW_NIL || tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
            return FU_TRUE;
        }
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_outer_doc_comment(FuToken tok) {
    assert(tok.kd == TOK_DOC_COMMENT);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok.sym);
    if (FuStr_get_char(symbol, 0) == '/') {
        return FU_TRUE;
    }
    if (FuStr_get_char(symbol, 0) == '!') {
        return FU_FALSE;
    }
    FATAL(NULL, "can not be here");
    return 0;
}

fu_bool_t FuToken_is_blank(FuToken tok) {
    switch (tok.kd) {
    case TOK_NEWLINE:
    case TOK_WHITESPACE:
    case TOK_COMMENT:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
        break;
    }
}

/* token 真正内容从哪里开始 */
fu_size_t FuToken_left_skip_count(FuToken tok) {
    fu_size_t count = 0;
    switch (tok.kd) {
    case TOK_STR:
        /* #..., " */
        count += tok._str.n_hashes + 1;
        break;
    case TOK_RAW_STR:
        /* r, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE_STR:
        /* b, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE_RAW_STR:
        /* b, r,  #..., " */
        count += 1 + 1 + tok._str.n_hashes + 1;
        break;
    case TOK_FORMAT_STR:
        /* f, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_FORMAT_RAW_STR:
        /* f, r,  #..., " */
        count += 1 + 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE:
    case TOK_CHAR:
    case TOK_INT:
    case TOK_FLOAT:
    default:
        FATAL(tok.sp, "can not be here");
    }
    return count;
}

FuStr *FuToken_display(FuToken tok) {
    FuStr *str = FuStr_new();
    char *kind_str = FuKind_token_cstr(tok.kd);
    FuStr_push_utf8_cstr(str, kind_str);
    if (tok.kd < TOK_DOC_COMMENT) {
        return str;
    }
    FuStr_push_utf8_cstr(str, ":");
    FuStr *symbol = FuStr_clone(FuCtx_get_symbol(tok.sp->ctx, tok.sym));
    FuStr_append(str, symbol);
    return str;
}

char *FuToken_kind_csr(FuToken tok) {
    if (tok.kd == TOK_KEYWORD) {
        return FuKind_keyword_cstr(tok.sym);
    }
    return FuKind_token_cstr(tok.kd);
}

/* clang-format off */
#define BIND(tok_kind, op_kind) \
    case tok_kind: {            \
        if (op) {               \
            *op = op_kind;      \
            return FU_TRUE;     \
        }                       \
        break;                  \
    }
/* clang-format on */

fu_bool_t FuToken_to_assign_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_EQ, OP_ASSIGN)
        BIND(TOK_PLUS_EQ, OP_ADD_ASSIGN)
        BIND(TOK_MINUS_EQ, OP_SUB_ASSIGN)
        BIND(TOK_STAR_EQ, OP_MUL_ASSIGN)
        BIND(TOK_SLASH_EQ, OP_DIV_ASSIGN)
        BIND(TOK_PERCENT_EQ, OP_REM_ASSIGN)
        BIND(TOK_AND_EQ, OP_BIT_AND_ASSIGN)
        BIND(TOK_OR_EQ, OP_BIT_OR_ASSIGN)
        BIND(TOK_CARET_EQ, OP_BIT_XOR_ASSIGN)
        BIND(TOK_SHL_EQ, OP_SHL_ASSIGN)
        BIND(TOK_SHR_EQ, OP_SHR_ASSIGN)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_prefix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
    case TOK_KEYWORD: {
        if (tok.sym == KW_AWAIT) {
            *op = OP_AWAIT;
            return FU_TRUE;
        }
        break;
    }
        BIND(TOK_STAR, OP_DEREF)
        BIND(TOK_NOT, OP_NOT)
        BIND(TOK_MINUS, OP_NEG)
        BIND(TOK_AND, OP_ADDRESS)
        BIND(TOK_DOT_DOT, OP_RANGE)
        BIND(TOK_DOT_DOT_EQ, OP_RANGE_EQ)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_infix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
    case TOK_KEYWORD: {
        if (tok.sym == KW_AS) {
            *op = OP_CAST;
            return FU_TRUE;
        }
        break;
    }
        BIND(TOK_PLUS, OP_ADD)
        BIND(TOK_MINUS, OP_SUB)
        BIND(TOK_STAR, OP_MUL)
        BIND(TOK_SLASH, OP_DIV)
        BIND(TOK_PERCENT, OP_REM)
        BIND(TOK_AND, OP_BIT_AND)
        BIND(TOK_OR, OP_BIT_OR)
        BIND(TOK_CARET, OP_BIT_XOR)
        BIND(TOK_SHL, OP_SHL)
        BIND(TOK_SHR, OP_SHR)
        BIND(TOK_LT, OP_LT)
        BIND(TOK_LE, OP_LE)
        BIND(TOK_GT, OP_GT)
        BIND(TOK_GE, OP_GE)
        BIND(TOK_EE, OP_EQ)
        BIND(TOK_NE, OP_NE)
        BIND(TOK_AND_AND, OP_AND)
        BIND(TOK_OR_OR, OP_OR)
        BIND(TOK_DOT_DOT, OP_RANGE)
        BIND(TOK_DOT_DOT_EQ, OP_RANGE_EQ)
        BIND(TOK_OPEN_PAREN, OP_CALL)
        BIND(TOK_OPEN_BRACKET, OP_INDEX)
        BIND(TOK_MOD_SEP, OP_STRUCT)
        BIND(TOK_DOT, OP_DOT)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_suffix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_QUESTION, OP_CATCH)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_prefix_ty_op(FuToken tok, fu_ty_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_STAR, TY_OP_PTR)
        BIND(TOK_OPEN_BRACKET, TY_OP_ARRAY)
        BIND(TOK_OPEN_PAREN, TY_OP_TUPLE)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_infix_ty_op(FuToken tok, fu_ty_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_RARROW, TY_OP_TRANS)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_suffix_ty_op(FuToken tok, fu_ty_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_QUESTION, TY_OP_NILABLE)
    default:
        break;
    }
    return FU_FALSE;
}

#undef BIND

FuLit *FuToken_to_lit_nil(FuToken tok) {
    assert(tok.kd == TOK_KEYWORD);
    assert(tok.sym == KW_NIL);
    return FuLit_new(tok.sp, LIT_NIL);
}

FuLit *FuToken_to_lit_bool(FuToken tok) {
    assert(tok.kd == TOK_KEYWORD);
    assert(tok.sym == KW_TRUE || tok.sym == KW_FALSE);
    FuLit *lit = FuLit_new(tok.sp, LIT_BOOL);
    if (tok.sym == KW_TRUE) {
        lit->_bool.v = 1;
    }
    if (tok.sym == KW_FALSE) {
        lit->_bool.v = 0;
    }
    return lit;
}

static fu_uint8_t Fu_decode_escape_byte(FuSpan *sp, FuStr *str, fu_size_t start, fu_size_t *offset,
                                        fu_bool_t check_len) {
    FuSpan *err_sp;
    /* start with backslash `\` */
    assert(FuStr_get_char(str, start) == '\\');
    fu_size_t str_len = FuStr_len(str);
    fu_size_t left_len = str_len - start;
    if (left_len < 2) {
        err_sp = FuSpan_offset(sp, *offset);
        FATAL(err_sp, "invalid byte escape sequences");
    }
    FuChar flag_char = FuStr_get_char(str, start + 1);
    *offset += 1;
/* clang-format off */
#define CHECK_STR_LEN(len)                                    \
    if (check_len && str_len > len) {                         \
        err_sp= FuSpan_offset(sp, *offset+1);                 \
        FATAL(err_sp, "invalid extra byte escape sequences"); \
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
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "byte escape sequences must be followed by 2 hex digits");
        }
        fu_uint8_t v = 0;
        fu_size_t i;
        for (i = 2; i < 4; i++) {
            FuChar bit = FuStr_get_char(str, start + i);
            if (bit == '\n') {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "byte escape sequences must be followed by 2 hex digits in the same line");
            }
            *offset += 1;
            if (!(FuChar_is_lowercase_hexadecimal_digit(bit))) {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "byte escape sequences must be lowercase hex digit");
            }
            v = v * 16 + FuChar_digit_to_uint(bit, 16);
        }
        CHECK_STR_LEN(4);
        return v;
        break;
    default:
        err_sp = FuSpan_offset(sp, *offset);
        FATAL(err_sp, "unknown byte escape sequences");
    }
#undef CHECK_STR_LEN
    /* can not be here */
    return 0;
}

FuLit *FuToken_to_lit_byte(FuToken tok) {
    assert(tok.kd == TOK_BYTE);
    if (!tok._byte.terminated) {
        FATAL(tok.sp, "mismatched char closing delimiter");
    }

    FuLit *lit = FuLit_new(tok.sp, LIT_BYTE);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._byte.sym);
    fu_size_t len = FuStr_len(symbol);
    if (len == 0) {
        FATAL(tok.sp, "empty byte");
    }
    FuChar first_char = FuStr_get_char(symbol, 0);
    if (first_char != '\\') {
        /* 'a' */
        if (FuStr_len(symbol) != 1) {
            FATAL(tok.sp, "byte must has one character");
        }
        if (!FuChar_is_ascii(first_char)) {
            FATAL(tok.sp, "byte must be ascii");
        }
        lit->_byte.v = first_char;
    } else {
        /* escape byte b'\n', b'\xff' */
        fu_size_t offset = 1;
        lit->_byte.v = Fu_decode_escape_byte(tok.sp, symbol, 0, &offset, FU_TRUE);
    }
    return lit;
}

FuLit *FuToken_to_lit_byte_str(FuToken tok) {
    FuSpan *err_sp;
    assert(tok.kd == TOK_BYTE_STR || tok.kd == TOK_BYTE_RAW_STR);
    if (!tok._str.started) {
        FATAL(tok.sp, "byte str needed open delimiter");
    }
    if (!tok._str.terminated) {
        FATAL(tok.sp, "mismatched byte str closing delimiter");
    }
    FuLit *lit = FuLit_new(tok.sp, LIT_BYTE_STR);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._str.sym);
    FuStr *fcontent = FuCtx_get_file(tok.sp->ctx, tok.sp->fpath);

    FuBytes *bytes = FuBytes_new();
    fu_size_t len = FuStr_len(symbol);

    FuChar fc;
    fu_size_t i = 0;
    fu_size_t offset = FuToken_left_skip_count(tok);
    if (tok._str.prefix_ignore) {
        FuChar fc = FuStr_get_char(fcontent, tok.sp->start + offset);
        if (fc != '\n') {
            err_sp = FuSpan_offset(tok.sp, offset);
            FATAL(err_sp, "byte str must started with newline");
        }
        offset += 1 + tok._str.prefix_ignore;
    }
    for (; i < len; i++) {
        if (tok.kd == TOK_BYTE_RAW_STR) {
            fc = FuStr_get_char(symbol, i);
            offset++;
            if (!FuChar_is_ascii(fc)) {
                err_sp = FuSpan_offset(tok.sp, offset);
                FATAL(err_sp, "byte must be ascii");
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
                err_sp = FuSpan_offset(tok.sp, offset);
                FATAL(err_sp, "byte must be ascii");
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
        fu_uint8_t byte = Fu_decode_escape_byte(tok.sp, symbol, i, &offset, FU_FALSE);
        FuBytes_push(bytes, byte);
        i += offset - old_offset - 1;
    }
    lit->_byte_str = bytes;
    return lit;
}

static FuChar Fu_decode_escape_char(FuSpan *sp, FuStr *str, fu_size_t start, fu_size_t *offset, fu_bool_t check_len) {
    FuSpan *err_sp;
    /* start with backslash `\` */
    assert(FuStr_get_char(str, start) == '\\');
    fu_size_t str_len = FuStr_len(str);
    fu_size_t left_len = str_len - start;
    if (left_len < 2) {
        err_sp = FuSpan_offset(sp, *offset);
        FATAL(err_sp, "invalid char escape sequences");
    }
    FuChar flag_char = FuStr_get_char(str, start + 1);
    *offset += 1;
/* clang-format off */
#define CHECK_STR_LEN(len)                                    \
    if (check_len && str_len > len) {                         \
        err_sp= FuSpan_offset(sp, *offset + 1);               \
        FATAL(err_sp, "invalid extra char escape sequences"); \
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
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "char escape sequences must be followed by 2 hex digits");
        }
        fu_uint8_t xv = 0;
        for (i = 2; i < 4; i++) {
            FuChar bit = FuStr_get_char(str, start + i);
            if (bit == '\n') {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "char escape sequences must be followed by 2 hex digits in the same line");
            }
            *offset += 1;
            if (!(FuChar_is_lowercase_hexadecimal_digit(bit))) {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "char escape sequences must be lowercase hex digit");
            }
            xv = xv * 16 + FuChar_digit_to_uint(bit, 16);
        }
        CHECK_STR_LEN(4);
        return xv;
        break;
    case 'u':
        /* '\u{a.....}' */
        if (left_len > 2 && FuStr_get_char(str, start + 2) == '\n') {
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "char escape sequences must enclosed by braces: `\\u{ff}` in the same line");
        }
        if (left_len > 2 && FuStr_get_char(str, start + 2) != '{') {
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "char escape sequences must use braces: `\\u{ff}`");
        }
        if (left_len < 5) {
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "char escape sequences must has 1-6 hex digits enclosed by braces: `\\u{ff}`");
        }
        fu_uint32_t uv = 0;
        for (i = 3; i < 9; i++) {
            FuChar bit = FuStr_get_char(str, i);
            *offset += 1;
            if (bit == '}') {
                break;
            }
            if (bit == '\n') {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "char escape sequences must be followed by 2 hex digits in the same line");
            }
            if (!FuChar_is_lowercase_hexadecimal_digit(bit)) {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "char escape sequences must be lowercase hex digit");
            }
            uv = uv * 16 + FuChar_digit_to_uint(bit, 16);
        }
        if (i == 9) {
            FuChar bit = FuStr_get_char(str, i);
            if (bit != '}') {
                err_sp = FuSpan_offset(sp, *offset);
                FATAL(err_sp, "char escape sequences must has 1-6 hex digits enclosed by braces: `\\u{ff}`");
            }
        }
        if (uv > 0x10FFFF) {
            err_sp = FuSpan_offset(sp, *offset);
            FATAL(err_sp, "invalid codepoint for \\u escape sequence");
        }
        return uv;
    default:
        err_sp = FuSpan_offset(sp, *offset);
        FATAL(err_sp, "unknown byte escape sequences");
    }
#undef CHECK_STR_LEN
    /* can not be here */
    return 0;
}

FuLit *FuToken_to_lit_char(FuToken tok) {
    assert(tok.kd == TOK_CHAR);
    if (!tok._char.terminated) {
        FATAL(tok.sp, "mismatched char closing delimiter");
    }

    FuLit *lit = FuLit_new(tok.sp, LIT_CHAR);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._char.sym);
    fu_size_t len = FuStr_len(symbol);
    if (len == 0) {
        FATAL(tok.sp, "empty char");
    }
    FuChar first_char = FuStr_get_char(symbol, 0);
    if (first_char != '\\') {
        /* 'a' */
        if (FuStr_len(symbol) != 1) {
            FATAL(tok.sp, "char must has one character");
        }
        lit->_char.v = first_char;
        return lit;
    } else {
        /* escape byte '\n', '\xff', '\u{ff}' */
        fu_size_t offset = 0;
        lit->_byte.v = Fu_decode_escape_char(tok.sp, symbol, 0, &offset, FU_TRUE);
    }
    return lit;
}

FuLit *FuToken_to_lit_str(FuToken tok) {
    FuSpan *err_sp;
    assert(tok.kd == TOK_STR || tok.kd == TOK_RAW_STR);
    if (!tok._str.started) {
        FATAL(tok.sp, "str needed open delimiter");
    }
    if (!tok._str.terminated) {
        FATAL(tok.sp, "mismatched str closing delimiter");
    }
    FuLit *lit = FuLit_new(tok.sp, LIT_STR);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._str.sym);
    FuStr *fcontent = FuCtx_get_file(tok.sp->ctx, tok.sp->fpath);

    FuStr *str = FuStr_new();
    fu_size_t len = FuStr_len(symbol);

    FuChar fc;
    fu_size_t i = 0;
    fu_size_t offset = FuToken_left_skip_count(tok);
    if (tok._str.prefix_ignore) {
        fc = FuStr_get_char(fcontent, tok.sp->start + offset);
        if (fc != '\n') {
            err_sp = FuSpan_offset(tok.sp, offset);
            FATAL(err_sp, "str must started with newline");
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
        FuChar c = Fu_decode_escape_char(tok.sp, symbol, i, &offset, FU_FALSE);
        FuStr_push(str, c);
        i += offset - old_offset - 1;
    }
    lit->_str = str;
    return lit;
}

static fu_uint64_t Fu_cstr_to_uint64(FuSpan *sp, char *cstr, fu_size_t base) {
    if (base < 2 || base > 36) {
        FATAL(sp, "invalid int base");
    }
    fu_uint64_t v, old_v;
    v = old_v = 0;
    char *p = cstr;
    while (*p) {
        old_v = v;
        v = v * base;
        if (v / base != old_v) {
            FATAL(sp, "too big int");
        }
        old_v = v;
        v += FuChar_digit_to_uint(*p, base);
        if (old_v > v) {
            FATAL(sp, "too big int");
        }
        p++;
    }
    return v;
}

static fu_err_t Fu_check_int_suffix(FuLit *lit, FuStr *suffix) {
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
    FuSpan *err_sp;
    assert(tok.kd == TOK_INT);
    FuLit *lit = FuLit_new(tok.sp, LIT_INT);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._str.sym);
    fu_size_t len = FuStr_len(symbol);

    if (tok._int.empty_int) {
        FATAL(tok.sp, "empty int");
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
#define ERR_DIGIT(kind)                      \
    err_sp= FuSpan_offset(tok.sp, i);        \
    FATAL(err_sp, "invalid " kind " digit");
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
            FATAL(tok.sp, "too big int");
        }
        buf[nbuf] = fc;
        nbuf++;
    }
    buf[nbuf] = 0;
    lit->_int.v = Fu_cstr_to_uint64(tok.sp, buf, tok._int.base);
    FuStr *suffix = FuStr_from_slice(symbol, tok._int.suffix_start, len);
    if (Fu_check_int_suffix(lit, suffix)) {
        err_sp = FuSpan_offset(tok.sp, tok._int.suffix_start);
        FATAL(err_sp, "invalid int suffix");
    }
    FuStr_drop(suffix);
    return lit;
}

FuLit *FuToken_to_lit_float(FuToken tok) {
    /* todo */
    FuLit *lit = FuLit_new(tok.sp, LIT_FLOAT);
    return lit;
}

FuLit *FuToken_to_lit_format_str(FuToken tok) {
    /* todo */
    FuLit *lit = FuLit_new(tok.sp, LIT_FORMAT_STR);
    return lit;
}

/* integer index convert */
FuIdent *FuToken_index_to_ident(FuToken tok) {
    assert(tok.kd == TOK_INT);
    if (tok._int.base != 10) {
        FATAL(tok.sp, "invalid index");
    }
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok._int.sym);
    if (FuStr_len(symbol) != tok._int.suffix_start) {
        FATAL(tok.sp, "invalid index");
    }
    return FuIdent_new(tok.sp, tok._int.sym);
}
